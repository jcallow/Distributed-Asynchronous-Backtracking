package com.john.system

import akka.actor.{Actor, ActorRef}
import com.john.variable._
import com.john.constraints.Constraint
import com.john.messages.Messages._
import scala.collection.mutable.{Set => MutableSet, Map => MutableMap}
import com.john.data.{PartialAssignment, NoGood, ConsistencyCheck, Consistent, Inconsistent}
import com.john.variable.{Assignment, Unassigned, Unknown}

class Agent(variable: Variable, constraints: Set[Constraint], higherAgents: Array[ActorRef], problemSize: Int) extends Actor { 
  println(higherAgents.size)
  val left = MutableMap[Int, ActorRef]() // agents we send to
  val right = MutableMap[Int, ActorRef]() // agents who send to us
  
  var currentAssignment = variable.domain(0)
  
  var view = (for (i <- 0 until variable.index) yield Assignment(i, Unassigned)).toArray
  
  var noGoods = Map[Array[Assignment], MutableSet[Assignment]]()
  
  constraints.map(c => c.variables.toSet).flatten.filter(v => v < variable.index).foreach(r => higherAgents(r) ! RequestLink(variable.index))
  
  println("Agent" + variable.index + " started!")
  
  checkAgentView
  
  def receive = {
    case message: Ok => processInfo(message)    
    case message: Conflict => resolveConflict(message.noGood, sender)
    case message: RequestLink => setLink(message.index, sender)
    case message: NoSolution => stop
    case message: Solution => check(message)
        
    case _ => {
      throw new Exception("I don't know what happened")
    }
  }
  
  def check(solution: Solution): Unit = consistent(solution.assignments(variable.index).assignment, solution.assignments.slice(0, variable.index)) match {
    case Consistent() => {
     // println("ok" + variable.index)
      if (variable.index == 0) {
        context.parent ! solution 
      }
      else higherAgents.last ! solution
    }
    case _ => return
  }

  
  def checkAgentView: Unit = consistent(currentAssignment, view) match {
    case NoGood(_) => {
      println("Agent" + variable.index + " found assignment inconsistent.")
      currentAssignment = chooseValue
      if (currentAssignment != Unassigned) {
        println("Agent" + variable.index + " found consistent assignment: " + (view :+ Assignment(variable.index, currentAssignment)).toList)
        
        if (variable.index+1 == problemSize) {
          val possibleSolution = view :+ Assignment(variable.index, currentAssignment)
          val allAssigned = possibleSolution.map(a => (a.assignment != Unknown)).reduce((a,b) => a && b)
          if (allAssigned) higherAgents.last ! Solution(view :+ Assignment(variable.index, currentAssignment))
        } 
        
        right.values.foreach(link => link ! Ok(Assignment(variable.index, currentAssignment)))
      } else backTrack
    }
    case Consistent() => {
      println("Agent" + variable.index + " found assignment consistent.")
      if (variable.index+1 == problemSize) {
        val possibleSolution = view :+ Assignment(variable.index, currentAssignment)
        val allAssigned = possibleSolution.map(a => (a.assignment != Unknown)).reduce((a,b) => a && b)
        if (allAssigned) higherAgents.last ! Solution(view :+ Assignment(variable.index, currentAssignment))
      }
      return
    }
  }
  
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
    println("Agent" + variable.index + " recieved ok: " + message.assignment)
    updateAgentView(message.assignment)
    checkAgentView
  }
  
  def resolveConflict(conflict: NoGood, sender: ActorRef) = {
    println("Agent" + variable.index + " resolving conflict with NoGood: " + conflict)
    if (coherant(conflict.noGood._1 :+ conflict.noGood._2, Set(variable.index) ++ left.keys)) {
      checkAddLink(conflict)
      if (noGoods.contains(conflict.noGood._1)) noGoods(conflict.noGood._1) += (conflict.noGood._2)
      else noGoods = noGoods + (conflict.noGood._1 -> MutableSet(conflict.noGood._2))
      currentAssignment = Unassigned
      checkAgentView
      println("Agent" + variable.index + "found conflict coherant, resolving")
    } else if (coherant(Array[Assignment](conflict.noGood._2), Set(variable.index))) {
      sender ! Ok(Assignment(variable.index, currentAssignment)) 
      println("Agent" + variable.index + "found conflict noncoherant, resending value")
    }
    println("Agent" + variable.index + " found conflict invalid")
  }
  
  def backTrack() = {
    val newNoGood = solve
    println("Agent" + variable.index + " backtracking with noGood: " + newNoGood)
    newNoGood match {
      case newNoGood: Inconsistent => stop
      case newNoGood: NoGood => {
        val sendTo = newNoGood.noGood._2.variable
        println(higherAgents(sendTo))
        
        higherAgents(sendTo) ! Conflict(newNoGood)
        updateAgentView(Assignment(newNoGood.noGood._2.variable, Unknown))
        checkAgentView
      }
      case _ => println("This should not have happened...")
    }
  }
  
  def solve: ConsistencyCheck = {
    
    val set = noGoods.keySet.flatten.toArray
    
    if (set.size == 0) return Inconsistent()
    else {
      val sorted = set.sortWith((a,b) => a.variable < b.variable)
      NoGood(sorted.slice(0, sorted.size-1), sorted.last)
    }
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
      if (variables.contains(a.variable)) {
        if (a.variable == variable.index && a.assignment != currentAssignment) return false
        else if (a.assignment != view(a.variable).assignment) return false
      }
    }
    true
  }
  
  def setLink(index: Int, ref: ActorRef) = {
    println("Agent" + variable.index + " adding link to " + ref)
    right.put(index, ref)
    ref ! Ok(Assignment(variable.index, currentAssignment))
  }
  
  def checkAddLink(noGood: NoGood) = {
    noGood.noGood._1.foreach( {case Assignment(index, assignment) => {
      if (!left.contains(index)) {
        higherAgents(index) ! RequestLink(variable.index)
        left.put(index, higherAgents(index))
        updateAgentView(Assignment(index, assignment))
      }
    }})
  }
  
  def stop = {
    println("Crashed")
    context.parent ! NoSolution
  }
  
}