package mud

import akka.actor.Actor
import akka.actor.ActorRef
import scala.util.Random

class NPC(
  name: String,
  item: String,
  itemSpec: (Int, Int)) //itemSpec tuples include (speed,damage) where speed (1-10) is the time in seconds/10 and damage (0-10) is the effectiveness of the weapon.
  extends Actor {

  import NPC._

  private var location: ActorRef = null
  private var health = 100
  val rand = new Random
  private var victim: ActorRef = null
  private var combatMode = false

  def receive = {
    case RandomMove =>
      if (!combatMode) {
        victim = null
        val r = rand.nextInt(6)
        //println("Random Move Called for NPC " + name)
        location ! Room.GetExit(r)
      }
    case Player.TakeExit(optRoom) =>
      if (!combatMode) {
        if (optRoom != None) {
          //print("NPC " + name + " Tried to move from " + location)
          location ! Room.RemovePlayer
          Main.playerManager ! PlayerManager.TellRoom(name.capitalize + " has left the room.", location)
          location = optRoom.get
          //println(" to " + location)
          location ! Room.AddPlayer
          Main.playerManager ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
        }
        Main.activityManager ! ActivityManager.Enqueue(RandomMove, rand.nextInt(75) + 50)
      }
    case Player.StartingRoom(room) =>
      location = room
      location ! Room.AddPlayer
      context.parent ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
      Main.activityManager ! ActivityManager.Enqueue(RandomMove, rand.nextInt(75) + 50)
    case Player.Attacked(otherPlayer) =>
      if (!combatMode) {
        victim = otherPlayer
        combatMode = true
      } else otherPlayer ! Player.PlayerBusy
    case Player.HitYou(oppHealth) =>
      if (combatMode){
        var damage = rand.nextInt(itemSpec._2 + 1)
        victim ! Player.HitMe(damage, item,health)
      }
    case Player.HitMe(damage, item,oppHealth) =>
      if (combatMode) {
        health -= damage
        if (health <= 0) {
          health = 0
          sender ! Player.OtherPlayerKilled(name.capitalize)
          removeNPCFromGame
        } else Main.activityManager ! ActivityManager.Enqueue(Player.HitYou(health), rand.nextInt(itemSpec._1 + 1))
      }
    case Player.OpponentFled =>
      combatMode = false
    case Player.OtherPlayerKilled(opponentName) =>
      context.parent ! PlayerManager.TellRoom(name.capitalize + " " + "killed " + opponentName, location)
      combatMode = false
    case Player.PlayerBusy =>
      combatMode = false
    case m => sender ! Player.PrintMessage("\n*****NPC received an unknown message: " + m + "*****")
  }

  def removeNPCFromGame: Unit = {
    //TODO Add all items of this player into the exit room
    location ! Room.RemovePlayer //Remove the player from the room so their ghost does not remain
    context.stop(self) //Kill the actor so people cannot send messages to dead actors
  }
}

object NPC {
  case class StartingRoom(room: ActorRef)
  case object RandomMove
}