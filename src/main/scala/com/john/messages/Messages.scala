package com.john.messages
import com.john.data.{PartialAssignment, Solution, NoGood}
import com.john.system.Network
import com.john.variable.Assignment

object Messages {
  
  case class Ok(assignment: Assignment) 
  
  case class Conflict(noGood: NoGood)
  
  case class Problem(network: Network) 
  
  case class Solution(assignments: Array[Assignment])
  
  case class NoSolution()
  
  case class RequestLink(index: Int)  
}