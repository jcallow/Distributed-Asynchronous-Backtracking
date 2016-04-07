package com.john.system

import akka.actor.{Actor, ActorRef, Props}
import com.john.messages.Messages._

class Master extends Actor {
  var initialized = 0
  var problemSize = 0
  var agents = Array[ActorRef]()
  
  def receive = {
    case message: Problem => {
      problemSize = message.network.variables.size
     // val agents = context.actorOf(Props(new Agent(variable, constraints)), "Agent_" + variable.index)
      
    }
    
    case message: Initialized => {
      initialized += 1
      if (problemSize == initialized) agents(0) ! Ok
    }
    
    case message: Ok => {
      println("Found Solution")
    }
    
    case _ => {
      throw new Exception("Solver recieved an unexpected message.")
    }
  }
}