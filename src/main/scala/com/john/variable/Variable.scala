package com.john.variable

import scala.collection.mutable.{ArrayBuffer => Array}

trait Variable {
  val index : Int
  val domain: Array[DomainValue]
}

case class FiniteVariable(index: Int, domain: Array[DomainValue]) extends Variable

// case class InfiniteDomain(index: Int, domain: Lazysomthing) extends Variable(index)