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
// 
// 

object Phonebook extends App {

  import PhonebookData._

  type Storage = (Int, Map[String, String])

  type Failure = String

  // We want some state
  type PhonebookState[A] = StateT[Id, Storage, A]

  // We want some error handling
  type PhonebookT[A] = EitherT[PhonebookState, Failure, A]

  def executeCommand(cmd: Command): PhonebookT[String] = liftStateTtoEitherT(cmd match {

    case Lookup(name) => for {
        s         <- tick()
        (_, book) =  s
        rslt      =  book.get(name).map( Right(_) ).getOrElse(Left("Not Found in phonebook"))
      } yield rslt

    case Remove(name) => for {
        s          <- tick()
        (cnt,book) =  s
        rslt        <- if (book.contains(name)) {
          put((cnt, book - name)).map(_ => Right("Successfully added %s to the book".format(name)))
        } else {
          init[Storage].map( _ => Left("Command %d failed: Can't remove %s from the book as no such record exists".format(cnt, name)))
        }
      } yield rslt

    case Add(name, information) => for {
      s           <- tick()
      (cnt, book) =  s
      rslt        <- if (book.contains(name)) {
        init[Storage].map( _ => Left("Command %d failed as %s is already in the book".format(cnt, name)))
      } else {
        put((cnt, book.updated(name, information))).map(_ => Right("Successfully added %s to the book".format(name)))
      }
    } yield rslt

  })

  def bulkExecute(cmds: List[Command]): PhonebookT[String] = cmds match {
    case Nil => liftStateTtoEitherT(for {
      s        <- init[Storage]
      (cnt, _) = s
      rslt     = Right("Successfully Executed All %d Commands".format(cnt))
    } yield rslt)

    case x :: xs => executeCommand(x) flatMap { _ => bulkExecute(xs) }
  }

  def tick(): PhonebookState[Storage] = for {
    s              <- init[Storage]
    (cnt, storage) =  s
    newS           <- put( (cnt+1, storage))
  } yield newS

  def liftStateTtoEitherT[A](st: PhonebookState[Either[Failure, A]]): PhonebookT[A] = 
    EitherT[PhonebookState, Failure, A](st)

  val initial = (0, HashMap[String, String]())
  
  println(
    bulkExecute(
      List(
        Add("mads","21"),
        Lookup("mads")
      )
    ).run.eval(initial)
  )

  println(
    bulkExecute(
      List(
        Remove("mads")
      )
    ).run.eval(initial)
  )

}

