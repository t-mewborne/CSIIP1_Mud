package mud

import akka.actor.Actor
import akka.actor.ActorRef

class ActivityManager extends Actor {
  import ActivityManager._
  val PQ = new PriorityQueue[Activity]((a1, a2) => a1.when - a2.when)

  var time = 0

  def receive = {
    case CheckQueue =>
      if (!PQ.isEmpty) {
        if (PQ.peek.when <= time) {
          val next = PQ.dequeue()
          next.who ! next.what
        }
        time += 1
      }
    case Enqueue(what, when) =>
      PQ.enqueue(new Activity(what, sender, when + time))
    case m => sender ! Player.PrintMessage("ActivityManager recieved unknown message: " + m)
  }
}

object ActivityManager {
  case class Activity(what: Any, who: ActorRef, when: Int)
  case object CheckQueue
  case class Enqueue(what: Any, when: Int)
}