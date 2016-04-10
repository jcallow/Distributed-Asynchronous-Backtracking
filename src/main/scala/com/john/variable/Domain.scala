package com.john.variable

abstract class DomainValue

case object Unassigned extends DomainValue

case object Unknown extends DomainValue

case class IntegerValue(value: Int) extends DomainValue