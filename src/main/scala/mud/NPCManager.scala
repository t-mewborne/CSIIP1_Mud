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
    case PrintNPC =>
      var npcs = "\n\nNPCs:\n"
      for (child <- context.children) npcs += (child.path.name + "\n")
      sender ! Player.PrintMessage(npcs)
    case GetNPCVictim(name) =>
      if (context.children.exists(_.path.name == name)) {
      for (child <- context.children) if (child.path.name == name) sender ! Player.TakeVictim(child)
      } else sender ! Player.PrintMessage("That player was not found room. (NPC Manager)")
    case m => sender ! Player.PrintMessage("NPCManager recieved unknown message: " + m + "\nFrom: " + sender)
  }
}

object NPCManager {
  case class newNPC(name: String, item: String, itemSpec: (Int,Int))
  case object PrintNPC
  case class GetNPCVictim(name:String)

}