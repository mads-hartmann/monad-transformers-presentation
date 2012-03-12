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

  type Storage = Map[Name, Information]

  type Failure = String

  // // We want some state
  type PhonebookState[A] = StateT[Id, Storage, A]

  type PhonebookT[A] = EitherT[PhonebookState, Failure, A]

  sealed abstract class Command
  case class Add(name: String, info: Information) extends Command
  case class Remove(name: String) extends Command
  case class Lookup(name: String) extends Command

  def executeCommand(cmd: Command): PhonebookT[Information] = cmd match {

    case Lookup(name) => liftStateTtoEitherT( for {
        s  <- init[Storage]
      } yield s.get(name).map( Right(_) ).getOrElse(Left("Not Found in phonebook")))

    case Remove(name) => liftStateTtoEitherT(for {
        s  <- init[Storage]
        _  <- put(s - name)
      } yield (Right("Successfully removed " + name): Either[Failure,Information]))

    case Add(name, information) => liftStateTtoEitherT(for {
      s <- init[Storage]
      _ <- put(s.updated(name, information))
    } yield (Right("Successfully added a person " + name): Either[Failure,Information]))

  }

  def bulkExecute(cmds: List[Command]): PhonebookT[Information] = cmds match {
    case Nil     => "Successfully Executed All Commands".point[PhonebookT]
    case x :: xs => executeCommand(x) flatMap { _ => bulkExecute(xs) }
  }

  def liftStateTtoEitherT[A](st: PhonebookState[Either[Failure, A]]): PhonebookT[A] = {
    EitherT[PhonebookState, Failure, A](st)
  }

  println(executeCommand(Lookup("mads")).run.eval(HashMap[Name, Information](("mads" -> "21"))))
  
  println(
    bulkExecute(
      List(
        Add("mads","21"),
        Lookup("mads")
      )
    ).run.eval(HashMap[Name, Information]())
  )

}

