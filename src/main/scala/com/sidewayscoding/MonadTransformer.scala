package com.sidewayscoding

import scalaz._
import Scalaz._
import scalaz.EitherT._
import scalaz.StateT._

import scala.collection.immutable.{ HashMap }

object PhonebookMonadTransformerApp extends App {

  import PhonebookData._

  type St = (Int, Map[String, String]) // The state of the application
  type Failure = String
  type PhonebookStateT[A] = StateT[Id, St, A] // We want some state
  type PhonebookT[A] = EitherT[PhonebookStateT, Failure, A] // We want some error handling

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

   def bulkExecute(cmds: List[Command]): PhonebookT[String] = cmds match {
     case Nil => liftStateTtoEitherT(for {
       s        <- init[St]
       (cnt, _) = s
       rslt     = Right("Successfully Executed All %d Commands".format(cnt))
     } yield rslt)
 
     case x :: xs => execute(x) flatMap { _ => bulkExecute(xs) }
   }

  def tick(): PhonebookStateT[St] = for {
    s              <- init[St]
    (cnt, storage) =  s
    newS           <- put( (cnt+1, storage))
  } yield newS

  // Can I get rid of the need for this somehow? 
  def liftStateTtoEitherT[A](st: PhonebookStateT[Either[Failure, A]]): PhonebookT[A] = 
    EitherT[PhonebookStateT, Failure, A](st)

  // 
  // Example
  // 

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
  
  val rslt3 = for { str <- bulkExecute(List(
    Add("mads","robo"),
    Add("michael","Greener"),
    Remove("mads"),
    Lookup("michael")
  ))} yield str
  
  println("rslt3:\n" + rslt3.run.eval(initial))

  // rslt1:
  // Right(Successfully added mads to the book
  // information for mads: 21)
  // rslt2:
  // Left(Failure executing command 1: Not Found in phonebook)

}

