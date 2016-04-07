package com.john.system

import akka.actor.{ActorRef, ActorSystem, Props}
import com.john.messages.Messages._

object Main {
  val system = ActorSystem("CSP_Solver")
  
  def start() = {
    val masterRef: ActorRef = system.actorOf(Props[Master], "Master")
    //masterRef ! problem()
    
    
    println("ok")
    system.terminate()
  }
  
  def main(args: Array[String]) {
    Main.start()
  }
}