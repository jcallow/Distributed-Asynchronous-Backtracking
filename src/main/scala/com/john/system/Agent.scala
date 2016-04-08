package com.john.system

import akka.actor.{Actor, ActorRef}
import com.john.variable._
import com.john.constraints.Constraint
import com.john.messages.Messages._
import scala.collection.mutable.{Set => MutableSet, Map => MutableMap}
import com.john.data.PartialAssignment
import com.john.variable.{Assignment, Unassigned}
import com.john.data.NoGood

class Agent(variable: Variable, constraints: MutableSet[Constraint], higherAgents: Array[ActorRef], problemSize: Int) extends Actor { 
  val links = MutableSet[ActorRef]() 
  
  var currentAssignment = variable.domain(0)
  
  var view = PartialAssignment(new Array[Assignment](variable.index+1))
  view.assignments(variable.index) = Assignment(variable.index, currentAssignment)
  
  var noGoods = Map[NoGood, DomainValue]()
  
  def receive = {
    case message: Ok => processInfo(message)    
    case message: Conflict => resolveConflict(message.noGood, sender)
    case message: RequestLink => setLink(sender)
    case message: NoSolution => stop
        
    case _ => {
      throw new Exception("I don't know what happened")
    }
  }

  
  def checkAgentView: Unit = {
    if (!(consistent(currentAssignment, view).isEmpty)) {
      currentAssignment = chooseValue
      if (currentAssignment != Unassigned) {
        links.foreach(link => link ! Ok(Assignment(variable.index, currentAssignment)))
      } else backTrack
    }
  }
  
  def consistent(currentValue: DomainValue, view: PartialAssignment): NoGood = {
    NoGood(Array[Assignment]())
  }
  
  def processInfo(message: Ok) = {
    updateAgentView(message.assignment)
    checkAgentView
  }
  
  def resolveConflict(conflict: NoGood, sender: ActorRef) = {
    if (coherant(conflict, Set(variable.index) ++ view.assignments.map(x => x.variable) )) {
      checkAddLink(conflict)
      noGoods = noGoods + (conflict -> currentAssignment)
      currentAssignment = Unassigned
      checkAgentView
    } else if (coherant(conflict, Set(variable.index))) sender ! Ok(Assignment(variable.index, currentAssignment)) 
  }
  
  def backTrack() = {
    val newNoGood = solve
    
    if (newNoGood.assignments.size == 0) stop
    else {
      val sendTo = newNoGood.assignments(newNoGood.assignments.size-1).variable
      higherAgents(sendTo) ! newNoGood
      updateAgentView(Assignment(sendTo, Unassigned))
      checkAgentView
    }
  }
  
  /*
   * Resolved the nogoods for backtracking.
   */
  def solve: NoGood = {
    val set = noGoods.keySet.flatMap(ng => ng.assignments.toSet).toArray
    val sorted = set.sortWith((a,b) => a.variable < b.variable)
    NoGood(sorted)
  }
  
  def chooseValue(): DomainValue = {
    variable.domain.filter(d => d != Unassigned)
    .foreach(d => {
      val ng = consistent(d, view)
      if (ng.isEmpty) return d
      else noGoods = noGoods + (ng -> d)
    })
    Unassigned
  }
  
  def updateAgentView(assignment: Assignment) = {
    view.assignments(assignment.variable) = assignment
    val viewIndices = view.assignments.map(x => x.variable)
    noGoods = noGoods.filterKeys(x => coherant(x, viewIndices.toSet))
  }
  
  def coherant(nogood: NoGood, variables: Set[Int]): Boolean = { 
    for (a <- nogood.assignments) {
      if (variables.contains(a.variable) && a.assignment != view.assignments(a.variable)) return false
    }
    true
  }
  
  def setLink(ref: ActorRef) = {
    links.add(ref)
    ref ! Ok(Assignment(variable.index, currentAssignment))
  }
  
  def checkAddLink(noGood: NoGood) = {
    noGood.assignments.foreach( {case Assignment(index, _) => {
      if (!links.contains(higherAgents(index))) higherAgents(index) ! RequestLink
    }})
  }
  
  def stop = {
    // No solution, send kill signal to master
  }
  
}