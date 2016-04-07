package com.john.system

import com.john.variable.Variable
import com.john.constraints.Constraint

case class Network(variables: Array[Variable], constraints: Set[Constraint])