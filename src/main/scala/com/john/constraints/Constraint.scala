package com.john.constraints

import com.john.data.PartialAssignment
import com.john.variable.Assignment

trait Constraint {
  def check(pa: Array[Assignment]): Boolean
  def variables: Array[Int]
}

case class NotEqual(var1: Int, var2: Int) extends Constraint {
  val variables = Array[Int](var1, var2).sorted
  
  def check(par: Array[Assignment]): Boolean = {
    par(var1).assignment != par(var2).assignment
  }
}