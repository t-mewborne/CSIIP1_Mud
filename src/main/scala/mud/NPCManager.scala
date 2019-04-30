package mud

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorRef

//THIS CLASS SHOULD ONLY TAKE ONE MESSAGE: newNPC
class NPCManager extends Actor {
  import NPCManager._

  def receive = {
    case newNPC(name, item, itemSpec) =>
      if (!context.children.exists(_.path.name == name)) {
        val NPC = context.actorOf(Props(new NPC(name, item, itemSpec)), name)
        Main.roomManager ! RoomManager.StartRoom(NPC)
      }
    case EnqueueNewNPC(name,item,itemSpec) =>
     Main.activityManager ! ActivityManager.Enqueue(newNPC(name,item,itemSpec), 2000)
    case PrintNPCAndPlayers(who, players) =>
      var npcs = "\nNPCs:\n"
      for (child <- context.children) npcs += (child.path.name.capitalize
        + "\n")
      who ! Player.PrintMessage(players + npcs)
    case m => sender ! Player.PrintMessage("\n*****NPCManager received an unknown message: " + m + "*****")
  }
}

object NPCManager {
  case class newNPC(name: String, item: String, itemSpec: (Int, Int))
  case class EnqueueNewNPC(name: String, item: String, itemSpec: (Int, Int))
  case class PrintNPCAndPlayers(who: ActorRef, players: String)
}