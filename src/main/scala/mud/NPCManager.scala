package mud

import akka.actor.Actor
import akka.actor.Props

//THIS CLASS SHOULD ONLY TAKE ONE MESSAGE: newNPC
class NPCManager extends Actor {
  import NPCManager._

  def receive = {
    case newNPC(name) =>
      if (!context.children.exists(_.path.name == name)) {
        val NPC = context.actorOf(Props(new NPC(name)), name)
        Main.roomManager ! RoomManager.StartRoom(NPC)
      }
    case PrintNPCs =>
      var npcs = "\n\nNPCs:\n"
      for (child <- context.children) npcs += (child.path.name + "\n")
      sender ! Player.PrintMessage(npcs)
    case m => sender ! Player.PrintMessage("NPCManager recieved unknown message: " + m)
  }
}

object NPCManager {
  case class newNPC(name: String)
  case object PrintNPCs
}