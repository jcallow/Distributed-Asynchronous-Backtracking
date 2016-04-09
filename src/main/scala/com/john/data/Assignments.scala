package com.john.data
import com.john.variable.Assignment

trait Assignments {
  
}

case class PartialAssignment(assignments: Array[Assignment]) extends Assignments

case class NoGood(noGood: (Array[Assignment], Set[Assignment])) extends Assignments {
  def isEmpty: Boolean = noGood._1.size == 0
}

case class Solution(assignments: Array[Assignment]) extends Assignments