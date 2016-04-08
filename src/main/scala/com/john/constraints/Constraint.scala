package com.john.constraints

import com.john.data.PartialAssignment

trait Constraint {
  def check(pa: PartialAssignment): Boolean
}
