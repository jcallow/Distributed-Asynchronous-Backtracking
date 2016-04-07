package com.john.variable

abstract class DomainValue

case object Unassigned extends DomainValue

case class IntegerValue(value: Int) extends DomainValue