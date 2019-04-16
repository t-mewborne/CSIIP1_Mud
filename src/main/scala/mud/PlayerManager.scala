package mud

import akka.actor.Actor
import java.io.PrintStream
import java.io.BufferedReader
import akka.actor.Props
import java.net.Socket
import akka.actor.ActorRef

class PlayerManager extends Actor {

  import PlayerManager._

  def receive = {
    case CheckAllInput =>
      for (child <- context.children) child ! Player.CheckInput
    case NewPlayer(in, out, name, sock) =>
      if (context.children.exists(_.path.name == name)) {
        out.println("\nThat name is already taken. Please try again with a different name.\n")
        out.close()
        in.close()
        sock.close()
      } else if (name.toLowerCase() == "cancel") {
        out.println("\nGoodbye!\n")
        out.close()
        in.close()
        sock.close()
      } else {
        val player = context.actorOf(Props(new Player(name, in, out, sock)), name)
        Main.roomManager ! RoomManager.StartRoom(player)
        out.println()
      }
    case SendMessageToAll(message) =>
      for (child <- context.children) {
        if (child != sender) child ! Player.PrintMessage(message)
      }
    case TellRoom(message, room) =>
      for (child <- context.children) {
        if (child != sender) child ! Player.PrintMessageRoom(message, room)
      }
    case TellPlayer(sendingPlayer, receivingPlayer, message) =>
      if (!context.children.exists(_.path.name == receivingPlayer)) {
        sender ! Player.PrintMessage("\nPlayer \"" + receivingPlayer.capitalize + "\" does not exist.\n")
      } else {
        for (child <- context.children) {
          if (child.path.name == receivingPlayer) {
            child ! Player.PrintMessage("\n\n" + sendingPlayer.capitalize + " sent you a message: \"" + message + "\"\n")
            sender ! Player.PrintMessage("\n\"" + message + "\" sent to " + receivingPlayer.capitalize + "\n")
          }
        }
      }
    case PrintPlayers =>
      var players = "\nPlayers:\n"
      for (child <- context.children) players += (child.path.name.capitalize + "\n")
      sender ! Player.PrintMessage(players)
    case m => sender ! Player.PrintMessage("PlayerManager recieved unknown message: " + m)
  }

}

object PlayerManager {
  case object CheckAllInput
  case class NewPlayer(in: BufferedReader, out: PrintStream, name: String, sock: Socket)
  case class SendMessageToAll(message: String)
  case class TellRoom(message: String, room: ActorRef)
  case class TellPlayer(sendingPlayer: String, receivingPlayer: String, message: String)
  case object PrintPlayers
}