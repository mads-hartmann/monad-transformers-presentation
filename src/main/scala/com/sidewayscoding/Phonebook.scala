package com.sidewayscoding

import scalaz._
import Scalaz._
import scalaz.EitherT._
import scalaz.StateT._
import scalaz.effect.IO
import scalaz.effect.IO.{ readLn, putStr, putStrLn, ioUnit }

import scala.collection.immutable.{ HashMap }

// Example Program using Monad Transformers: A Phonebook 
// 
// I've chosen to model a phonebook because it's simple
// but still needs to deal with state and failures, the
// two things I've focused on in my presentation. 

object PhonebookMonadTransformerApp extends App {

  import PhonebookData._

  type St = (Int, Map[String, String])

  type Failure = String

  // We want some state
  type PhonebookStateT[A] = StateT[Id, St, A]

  // We want some error handling
  type PhonebookT[A] = EitherT[PhonebookStateT, Failure, A]

  def execute(cmd: Command): PhonebookT[String] = liftStateTtoEitherT(cmd match {

    case Lookup(name) => for {
        s         <- tick()
        (cnt, book) =  s
        rslt      =  book.get(name).map( x => Right("information for mads: " + x) )
                                   .getOrElse(Left("Failure executing command %d: Not Found in phonebook".format(cnt)))
      } yield rslt

    case Remove(name) => for {
        _          <- tick()
        rslt       <- modify { (s: St) => (s._1, s._2 - name)}
      } yield Right("Successfully removed %s to the book".format(name))

    case Add(name, information) => for {
      s     <- tick()
      rslt  <- modify { (s: St) => (s._1, s._2 + (name -> information)) }
    } yield Right("Successfully added %s to the book".format(name))

  })

  def tick(): PhonebookStateT[St] = for {
    s              <- init[St]
    (cnt, storage) =  s
    newS           <- put( (cnt+1, storage))
  } yield newS

  def liftStateTtoEitherT[A](st: PhonebookStateT[Either[Failure, A]]): PhonebookT[A] = 
    EitherT[PhonebookStateT, Failure, A](st)

  val initial = (0, HashMap[String, String]())
  
  val rslt1 = for {
    msg1 <- execute(Add("mads","21"))
    msg2 <- execute(Lookup("mads"))
  } yield List(msg1, msg2).mkString("\n")

  println("rslt1:\n" + rslt1.run.eval(initial))

  val rslt2 = for {
    msg1 <- execute(Lookup("mads"))
    msg2 <- execute(Add("mads","21"))
  } yield List(msg1, msg2).mkString("\n")

  println("rslt2:\n" + rslt2.run.eval(initial))

}

