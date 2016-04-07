package com.john.messages
import com.john.data.{PartialAssignment, Solution}
import akka.actor.{ActorRef}
import com.john.system.Network
import com.john.variable.Assignment

object Messages {
  
  case class Ok(assignment: Assignment) 
  
  case class NoGood(assignements: PartialAssignment)
  
  case class Problem(network: Network) 
  
  case class NoSolution()
  
  case class Request()
  
  case class Initialize(link: ActorRef, agents: Array[ActorRef])
  
  case class Initialized()
  
}