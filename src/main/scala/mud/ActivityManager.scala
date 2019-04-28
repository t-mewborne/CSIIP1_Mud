package mud

import akka.actor.Actor
import akka.actor.ActorRef

class ActivityManager extends Actor {
  import ActivityManager._
  val PQ = new PriorityQueue[Activity]((a1, a2) => a2.when - a1.when)

  var time = 0

  def receive = {
    case CheckQueue =>
      if (!PQ.isEmpty) {
        if (PQ.peek.when <= time) {
          val next = PQ.dequeue()
          next.who ! next.what
        } 
        //else println("Next Activity:\tWhat:" + PQ.peek.what + "\t When:" + (PQ.peek.when) + "\t\t Current Time:" + time + "\t\tWho:" + PQ.peek.who)
        time += 1
      } else time = 0
    case Enqueue(what, when) =>
      PQ.enqueue(new Activity(what, sender, when + time))
      //println("Activity Added:\tWhat:" + what + "\t When:" + (when+time) + "\t\t Current Time:" + time + "\t\tWho:" + sender)
    case m => sender ! Player.PrintMessage("\n*****ActivityManager received an unknown message: " + m + "*****")
  }
}

object ActivityManager {
  case class Activity(what: Any, who: ActorRef, when: Int)
  case object CheckQueue
  case class Enqueue(what: Any, when: Int)
}