package com.john.problems

import com.john.system.Network
import com.john.variable.FiniteVariable
import com.john.variable.IntegerValue
import com.john.constraints.NotEqual
import com.john.variable.Variable
import com.john.constraints.Constraint

object nQueens {
  
  def getProblem(n: Int): Network = {
    
    val v1 = FiniteVariable(0, Array(IntegerValue(0), IntegerValue(1)))
    val v2 = FiniteVariable(1, Array(IntegerValue(0), IntegerValue(1)))
    
    val c1 = NotEqual(0, 1)
    
    val variables = Array[Variable](v1, v2)
    val constraints = Set[Constraint](c1)
    
    
    Network(variables, constraints)
    
    
  }
  
}