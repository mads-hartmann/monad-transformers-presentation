package com.sidewayscoding

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
/*

import com.sidewayscoding._

for { 
  a <- Success(10) 
  b <- Fail[Int]() 
} yield a + b

Success(10).flatMap( a => Fail[Int].map( b => a+b )) 

Success(10).flatMap( a => Fail[Int].map( b => a+b )).getOrElse(42)

for { 
  a <- Success(10)
  b <- Success(20) 
} yield a + b

Success(10).flatMap( a => Success(20).map( b => a+b )) 

Success(10).flatMap( a => Success(20).map( b => a+b )).getOrElse(42)
*/

object OptionExample extends App {
  
  import scala.collection.immutable.{ HashMap }

  // Using Option here rather than Maybe because 
  // I don't want to implement my own map implementation

  val people = HashMap(
    ("Douglas Adams" -> 0), 
    ("Akira Toriyama" -> 1)
  )

  val awesomeness = HashMap(
    (0 -> 42),
    (1 -> 9001)
  )

  val howAwesomeIsDouglas = for {
    id <- people.get("Douglas Adams")
    awesomeScore <- awesomeness.get(id)
  } yield awesomeScore

  val howAwesomeIsMads = for {
    id <- people.get("Mads Hartmann")
    awesomeScore <- awesomeness.get(id)
  } yield awesomeScore

  println("Douglas has an awesomeness factor of: %s".format( howAwesomeIsDouglas.getOrElse("N/A") ))
  println("Mads has an awesomeness factor of: %s".format( howAwesomeIsMads.getOrElse("N/A") ))

}


