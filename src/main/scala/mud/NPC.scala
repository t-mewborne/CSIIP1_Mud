package mud

import akka.actor.Actor
import akka.actor.ActorRef
import scala.util.Random

class NPC(name: String) extends Actor {

  import NPC._
  private var location: ActorRef = null

  val rand = new Random
  
  def receive = {
    case RandomMove =>
      val r = rand.nextInt(6)
      location ! Room.GetExit(r)
    case Player.TakeExit(optRoom) =>
      if (optRoom != None) {
        location ! Room.RemovePlayer(name)
        Main.playerManager ! PlayerManager.TellRoom(name.capitalize + " has left the room.", location)
        location = optRoom.get
        location ! Room.AddPlayer(name)
        Main.playerManager ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
        location ! Room.GetDetails
      }
      Main.activityManager ! ActivityManager.Enqueue(RandomMove,rand.nextInt(75)+50)
    case Player.StartingRoom(room) =>
      location = room
      location ! Room.AddPlayer(name)
      context.parent ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
      Main.activityManager ! ActivityManager.Enqueue(RandomMove,rand.nextInt(75)+50)
    case m => sender ! Player.PrintMessage("NPC received an unknown message: " + m)
  }
}

object NPC {
  case class StartingRoom(room: ActorRef)
  case object RandomMove
}