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
  
  var currentAssignment = Assignment(variable, variable.domain(0))
  
  var view:PartialAssignment = PartialAssignment(new Array[Assignment](variable.index+1))
  view.assignments(variable.index) = currentAssignment
  
  def receive = {
    case message: Ok => processInfo(message)    
    case message: NoGood => resolveConflict(message)
    case message: RequestLink => setLink(sender)
    case message: NoSolution => stop
    
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
      v
      index += 1
    }
  }
  
  def processInfo(message: Ok) = {
    updateAgentView(message.assignment)
    checkAgentView
  }
  
  def resolveConflict(message: NoGood) = {
    
  }
  
  def backTrack() = {
    
  }
  
  def chooseValue(): DomainValue = {
    Unassigned
  }
  
  def updateAgentView(assignment: Assignment) = {
    
  }
  
  def coherant(nogood: NoGood, agents: Variable): Boolean = {
    false 
  }
  
  def setLink(ref: ActorRef) = {
    
  }
  
  def checkAddLink() = {
    
  }
  
}