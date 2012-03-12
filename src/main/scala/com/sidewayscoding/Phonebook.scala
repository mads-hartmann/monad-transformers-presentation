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

  type Name = String

  type Information = String

  type Storage = (Int, Map[Name, Information])

  type Failure = String

  // We want some state
  type PhonebookState[A] = StateT[Id, Storage, A]

  // We want some error handling
  type PhonebookT[A] = EitherT[PhonebookState, Failure, A]

  sealed abstract class Command
  case class Add(name: String, info: Information) extends Command
  case class Remove(name: String) extends Command
  case class Lookup(name: String) extends Command

  def executeCommand(cmd: Command): PhonebookT[Information] = liftStateTtoEitherT(cmd match {

    case Lookup(name) => for {
        s         <- tick()
        (_, book) =  s
        rslt      =  book.get(name).map( Right(_) ).getOrElse(Left("Not Found in phonebook"))
      } yield rslt

    case Remove(name) => for {
        s          <- tick()
        (cnt,book) =  s
        _          <- put( (cnt, book - name) )
        rslt       =  Right("Successfully removed " + name)
      } yield rslt

    case Add(name, information) => for {
      s           <- tick()
      (cnt, book) =  s
      _           <- put( (cnt, book.updated(name, information)) )
      rslt        =  Right("Successfully added a person " + name)
    } yield rslt

  })

  def bulkExecute(cmds: List[Command]): PhonebookT[Information] = cmds match {
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

  println(executeCommand(Lookup("mads")).run.eval(
    (0, HashMap[Name, Information](("mads" -> "21")))
  ))
  
  println(
    bulkExecute(
      List(
        Add("mads","21"),
        Lookup("mads")
      )
    ).run.eval(
      (0, HashMap[Name, Information]())
    )
  )

}
