package com.john.system

import akka.actor.{Actor, ActorRef}
import com.john.variable._
import com.john.constraints.Constraint
import com.john.messages.Messages._
import scala.collection.mutable.{Set => MutableSet, Map => MutableMap}
import com.john.data.{PartialAssignment, NoGood, ConsistencyCheck, Consistent}
import com.john.variable.{Assignment, Unassigned}

class Agent(variable: Variable, constraints: Set[Constraint], higherAgents: Array[ActorRef], problemSize: Int) extends Actor { 
  val links = MutableSet[ActorRef]() 
  
  var currentAssignment = variable.domain(0)
  
  var view = new Array[Assignment](variable.index)
  
  var noGoods = Map[Array[Assignment], MutableSet[Assignment]]()
  
  def receive = {
    case message: Ok => processInfo(message)    
    case message: Conflict => resolveConflict(message.noGood, sender)
    case message: RequestLink => setLink(sender)
    case message: NoSolution => stop
        
    case _ => {
      throw new Exception("I don't know what happened")
    }
  }

  
  def checkAgentView: Unit = consistent(currentAssignment, view) match {
    case NoGood(_) => {
      currentAssignment = chooseValue
      if (currentAssignment != Unassigned) {
        links.foreach(link => link ! Ok(Assignment(variable.index, currentAssignment)))
      } else backTrack
    }
    case Consistent() => return
  }
  
  
  /*
   * Finish me
   */
  def consistent(currentValue: DomainValue, view: Array[Assignment]): ConsistencyCheck = {
    val set = MutableSet[Int]()
    for (constraint <- constraints) {
      if(!constraint.check(view :+ Assignment(variable.index, currentValue))) set ++= constraint.variables
    }
    if (set.isEmpty) return Consistent()
    else {
      set -= variable.index
      val left = set.toArray.sorted.map(index => view(index))
      val right = Assignment(variable.index, currentValue)
      return NoGood(left, right)
    }
  }
  
  def processInfo(message: Ok) = {
    updateAgentView(message.assignment)
    checkAgentView
  }
  
  def resolveConflict(conflict: NoGood, sender: ActorRef) = {
    if (coherant(conflict.noGood._1 :+ conflict.noGood._2, Set(variable.index) ++ view.map(x => x.variable) )) {
      checkAddLink(conflict)
      if (noGoods.contains(conflict.noGood._1)) noGoods(conflict.noGood._1) += (conflict.noGood._2)
      else noGoods = noGoods + (conflict.noGood._1 -> MutableSet(conflict.noGood._2))
      currentAssignment = Unassigned
      checkAgentView
    } else if (coherant(Array[Assignment](conflict.noGood._2), Set(variable.index))) sender ! Ok(Assignment(variable.index, currentAssignment)) 
  }
  
  def backTrack() = {
    val newNoGood = solve
    
    if (newNoGood.isEmpty) stop
    else {
      val sendTo = newNoGood.noGood._2.variable
      higherAgents(sendTo) ! newNoGood
      updateAgentView(newNoGood.noGood._2)
      checkAgentView
    }
  }
  
  def solve: NoGood = {
    val set = noGoods.keySet.flatten.toArray
    val sorted = set.sortWith((a,b) => a.variable < b.variable)
    NoGood(sorted.slice(0, sorted.size-1), sorted.last)
  }
  
  def chooseValue(): DomainValue = {
    val eliminated = noGoods.values.flatten.map(a => a.assignment).toSet
    
    variable.domain.filter(d => !eliminated.contains(d))
    .foreach(d => {
      val ng = consistent(d, view)
      ng match {
        case Consistent() => return d
        case NoGood(noGood) => {
          if (noGoods.contains(noGood._1)) noGoods(noGood._1).add(noGood._2)
          else noGoods = noGoods + (noGood._1 -> MutableSet(noGood._2))
        }
      }      
    })
    Unassigned
  }
  
  def updateAgentView(assignment: Assignment) = {
    view(assignment.variable) = assignment
    val viewIndices = view.map(x => x.variable)
    noGoods = noGoods.filterKeys(x => coherant(x, viewIndices.toSet))
  }
  
  def coherant(assignments: Array[Assignment], variables: Set[Int]): Boolean = { 
    for (a <- assignments) {
      if (variables.contains(a.variable) && a.assignment != view(a.variable).assignment) return false
    }
    true
  }
  
  def setLink(ref: ActorRef) = {
    links.add(ref)
    ref ! Ok(Assignment(variable.index, currentAssignment))
  }
  
  def checkAddLink(noGood: NoGood) = {
    noGood.noGood._1.foreach( {case Assignment(index, _) => {
      if (!links.contains(higherAgents(index))) higherAgents(index) ! RequestLink
    }})
  }
  
  def stop = {
    println("Can't find solution")
    // No solution, send kill signal to master
  }
  
}