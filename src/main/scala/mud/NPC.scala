package mud

import akka.actor.Actor
import akka.actor.ActorRef
import scala.util.Random

class NPC(name: String, 
    item:String, 
    itemSpec:(Int,Int)) //itemSpec tuples include (speed,damage) where speed (1-10) is the time in seconds/10 and damage (0-10) is the effectiveness of the weapon.
    extends Actor {

  import NPC._
  
  private var location: ActorRef = null
  private var health = 100
  val rand = new Random
  
  def receive = {
    case RandomMove =>
      val r = rand.nextInt(6)
      //println("Random Move Called for NPC " + name)
      location ! Room.GetExit(r)
    case Player.TakeExit(optRoom) =>
      if (optRoom != None) {
        //print("NPC " + name + " Tried to move from " + location)
        location ! Room.RemovePlayer(name)
        Main.playerManager ! PlayerManager.TellRoom(name.capitalize + " has left the room.", location)
        location = optRoom.get
        //println(" to " + location)
        location ! Room.AddPlayer(name)
        Main.playerManager ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
      }
      Main.activityManager ! ActivityManager.Enqueue(RandomMove,rand.nextInt(75)+50)
    case Player.StartingRoom(room) =>
      location = room
      location ! Room.AddPlayer(name)
      context.parent ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
      Main.activityManager ! ActivityManager.Enqueue(RandomMove,rand.nextInt(75)+50)
      //println("Created NPC \t" + name + "\t\twith a starting room of \t" + location)
    case m => sender ! Player.PrintMessage("NPC received an unknown message: " + m)
  }
}

object NPC {
  case class StartingRoom(room: ActorRef)
  case object RandomMove
}