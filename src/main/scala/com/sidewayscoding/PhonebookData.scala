package com.sidewayscoding

object PhonebookData {
  sealed abstract class Command
  case class Add(name: String, info: String) extends Command
  case class Remove(name: String) extends Command
  case class Lookup(name: String) extends Command
}