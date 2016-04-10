package com.john.system

import akka.actor.{Actor, ActorRef, Props}
import com.john.messages.Messages._
import akka.actor.Kill
import scala.collection.mutable.{ArrayBuffer => MutableArray}
import com.john.constraints.Constraint

class Master extends Actor {
  var initialized = 0
  var problemSize = 0
  var agents = Array[ActorRef]()
  
  def receive = {
    case message: Problem => {
      println("Problem recieved")
      val problemSize = message.network.variables.size
      val variables = message.network.variables
      val constraints = message.network.constraints
      val constraintMap = constraints.map(c => (c.evaluator, c)).groupBy(_._1).mapValues(_.map(_._2))
      
      var agents = Array[ActorRef]()
      
      for (i <- 0 until problemSize) {
        val higherAgents = agents
        val consts = if (constraintMap.contains(i)) constraintMap(i) else Set[Constraint]()
        val agent = context.actorOf(Props(new Agent(variables(i), consts, higherAgents, problemSize)), "Agent+" + i)
        agents = agents :+ agent
      }
      
    }
    
    case message: Solution => {
      println("Found Solution: " + Solution)
      
      context.children.foreach(c => c ! Kill)
      context.stop(self)
    }
    
    case message: NoSolution => {
      println("No solution, killing all agents")
      
      context.children.foreach(c => c ! Kill)
      context.stop(self)
    }
    
    case _ => {
      throw new Exception("Solver recieved an unexpected message.")
    }
  }
}