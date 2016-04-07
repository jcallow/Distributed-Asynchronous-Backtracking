package com.john.system

import akka.actor.{Actor, ActorRef}
import com.john.variable._
import com.john.constraints.Constraint
import com.john.messages.Messages._
import scala.collection.mutable.{Set => MutableSet}
import com.john.data.PartialAssignment
import com.john.variable.{Assignment, Unassigned}
import com.john.constraints.{NoGood => CNoGood}

class Agent(variable: Variable, constraints: MutableSet[Constraint], problemSize: Int) extends Actor {
  var agents: Array[ActorRef] = null
  val links = MutableSet[ActorRef]() 
  var view:PartialAssignment = PartialAssignment(new Array[Assignment](variable.index+1))
  view.assignments(variable.index) = variable match {
    case FiniteVariable(index, domain) => Assignment(variable, domain(0))
  }
  
  def receive = {
    case message: Ok => {
      val variable = message.assignment.variable
      view.assignments(variable.index) = message.assignment
      checkAgentView
    }
    
    case message: NoGood => {
      constraints.add(CNoGood(message.assignements))
    }
    
    case message: NoSolution => {
      // terminate actor 
    }
    
    case message: Request => {
      links.add(sender)
    }
    
    case message: Initialize => {
      links.add(message.link)
      agents = message.agents
      sender ! Initialized()
    }
    
    case _ => {
      throw new Exception("I don't know what happened")
    }
  }
  
  def checkAgentView = {
    var index = 0
    while(index < variable.domain.size) {
      // check consistency
      index += 1
    }
  }
  
  def backTrack = {
    // Create NoGood message
    
    // if NoGood is empty broadcast NoSolution
    
    
  }
  
}