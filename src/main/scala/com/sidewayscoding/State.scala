package com.sidewayscoding

case class State[S,R](private val f: (S) => (R,S)) {

  def map[A](g: (R) => A): State[S,A] = State { (s: S) => 
    val (a, s2) = this.f(s)
    (g(a), s2) 
  }

  def flatMap[A](g: R => State[S,A]): State[S,A] = 
    State { (s: S) => 
      val (a, s2) = this.f(s)
      val State(f2) = g(a)
      f2(s2)
    }

  def eval(s: S) = this.f(s)._1

  def exec(s: S) = this.f(s)._2

}

object State {

  def get[S] = State[S,S]( (s: S) => (s,s) )

  def put[S](newS: S) = modify( (_: S) => newS)

  def modify[S]( g: (S) => S) = State[S,S] { (s) => 
    val newS = g(s)
    (newS, newS)
  }

}

object PhonebookStateApp extends App {
  
  import PhonebookData._
  import State._
  import scala.collection.immutable.{ HashMap }

  type Storage = HashMap[String,String]

  def execute(cmd: Command): State[Storage, String] = cmd match {

    case Add(name, info) => for {
      _ <- modify { (s: Storage) => s.updated(name, info) }
    } yield "Added new record: %s".format(name)

    case Remove(name) => for {
      _ <- modify { (s: Storage) => s - name }
    } yield "Removed record: %s".format(name)

    case Lookup(name) => for {
      s <- get[Storage]
    } yield s.get(name).map( info => "Information for %s: %s".format(name, info)).getOrElse("No such person in the book")
  }

  // 
  // Example usage
  //

  val initialStorage = HashMap[String,String]()

  val rslt1 = for {
    str1 <- execute(Add("mads","DIKU"))
    str2 <- execute(Lookup("mads"))
    str3 <- execute(Remove("mads"))
    str4 <- execute(Lookup("mads"))
    str5 <- execute(Add("mads","DIKU"))
  } yield List(str1, str2, str3, str4, str5).mkString("\n")

  println("rslt1:\n" + rslt1.eval(initialStorage))
  // rslt1:
  // Added new record: mads
  // Information for mads: DIKU
  // Removed record: mads
  // No such person in the book
  // Added new record: mads

}
