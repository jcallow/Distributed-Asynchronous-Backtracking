package com.john.data
import com.john.variable.Assignment
import com.john.variable.Unassigned

trait Assignments 

case class PartialAssignment(assignments: Array[Assignment]) extends Assignments

case class Solution(assignments: Array[Assignment]) extends Assignments

trait ConsistencyCheck

case class NoGood(noGood: (Array[Assignment], Assignment)) extends ConsistencyCheck {
  def isEmpty: Boolean = (noGood._1.size == 0)
}

case class Consistent() extends ConsistencyCheck