package com.john.system

import akka.actor.{ActorRef, ActorSystem, Props}
import com.john.messages.Messages._
import com.john.variable.FiniteVariable
import com.john.problems.nQueens

object Main {
  val system = ActorSystem("CSP_Solver")
  
  def start() = {
    val masterRef: ActorRef = system.actorOf(Props[Master], "Master")
    
    val network = nQueens.getProblem(1)
    masterRef ! Problem(network)
    println("ok")
   // system.terminate()
  }
  
  def main(args: Array[String]) {
    Main.start()
  }
}