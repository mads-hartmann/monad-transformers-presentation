package com.sidewayscoding

import scala.collection.immutable.{ HashMap }

abstract class Maybe[A] {

  def map[B](f: A => B): Maybe[B]
  def flatMap[B](f: A => Maybe[B]): Maybe[B]
  def getOrElse(default: A): A

}

case class Success[A](private val value: A) extends Maybe[A] {

  def map[B](f: A => B) = Success(f(this.value))
  def flatMap[B](f: A => Maybe[B]) = f(this.value)
  def getOrElse(default: A) = this.value

}

case class Fail[A] extends Maybe[A] {

  def map[B](f: A => B) = Fail[B]()
  def flatMap[B](f: A => Maybe[B]) = Fail[B]()
  def getOrElse(default: A) = default

}

object PhonebookOptionApp extends App {
  
  import PhonebookData._
  
  type Storage = HashMap[String,String]

  def execute(cmd: Command, storage: Storage): Option[(Storage,String)] = cmd match {

    case Add(name, info) => Some(storage + (name -> info), "Added new record: %s".format(name))
    case Remove(name)    => Some(storage - name, "Removed record: %s".format(name))
    case Lookup(name)    => storage.get(name)
                                   .map( info => (storage,"Information for %s: %s".format(name, info)))
  }

  // 
  // Example usage
  // 

  val initialStorage = HashMap[String,String]()

  val rslt1 = for {
    tup1 <- execute(Add("mads","DIKU"), initialStorage)
    tup2 <- execute(Lookup("mads"), tup1._1)
  } yield List(tup1._2, tup2._2).mkString("\n")

  val rslt2 = for {
    tup1 <- execute(Add("mads","DIKU"), initialStorage)
    tup2 <- execute(Lookup("mads"), tup1._1)
    tup3 <- execute(Remove("mads"), tup2._1)
    tup4 <- execute(Lookup("mads"), tup3._1)
  } yield List(tup1._2, tup2._2, tup3._2, tup4._2).mkString("\n")

  println("rslt1:\n" + rslt1.getOrElse("failed ;-("))
  println("rslt2:\n" + rslt2.getOrElse("failed ;-("))

}


