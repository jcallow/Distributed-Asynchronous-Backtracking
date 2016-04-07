package com.john.constraints

import com.john.data.PartialAssignment

trait Constraint {
  def check(pa: PartialAssignment): Boolean
}

case class NoGood(noGood: PartialAssignment) extends Constraint {
  override def check(pa: PartialAssignment): Boolean = {
    val t = noGood.assignments.foreach(b => {
       if (pa.assignments(b.variable.index).assignment != b.assignment) return false
    })
    true
  }  
}