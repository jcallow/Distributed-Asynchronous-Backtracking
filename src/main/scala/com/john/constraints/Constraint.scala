package com.john.constraints

import com.john.data.PartialAssignment

trait Constraint {
  def check(pa: PartialAssignment): Boolean
  def variables: Array[Int]
}

case class NotEqual(var1: Int, var2: Int) extends Constraint {
  val variables = Array[Int](var1, var2).sorted
  
  def check(par: PartialAssignment): Boolean = {
    par.assignments(var1).assignment != par.assignments(var2).assignment
  }
}