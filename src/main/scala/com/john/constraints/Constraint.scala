package com.john.constraints

import com.john.data.PartialAssignment
import com.john.variable._

trait Constraint {
  def check(pa: Array[Assignment]): Boolean
  def variables: Array[Int]
  def evaluator: Int
}

case class NotEqual(var1: Int, var2: Int) extends Constraint {
  val variables = Array[Int](var1, var2).sorted
  val evaluator = variables(1)
  
  def check(par: Array[Assignment]): Boolean = {
    if (par(var1).assignment == Unassigned || par(var2).assignment == Unassigned) return false
    if (par(var1).assignment == Unknown || par(var2).assignment == Unknown) return true
    par(var1).assignment != par(var2).assignment
  }
}


case class NotDiagonal(var1: Int, var2: Int) extends Constraint {
  val variables = Array[Int](var1, var2).sorted
  val evaluator = variables(1)
  
  def check(par: Array[Assignment]): Boolean = {
    if (par(var1).assignment == Unassigned || par(var2).assignment == Unassigned) return false
    if (par(var1).assignment == Unknown || par(var2).assignment == Unknown) return true
    par(var1).assignment != par(var2).assignment
  }
}