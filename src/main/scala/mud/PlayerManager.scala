package mud

import akka.actor.Actor
import java.io.PrintStream
import java.io.BufferedReader
import akka.actor.Props

class PlayerManager extends Actor{
  
  import PlayerManager._
  
  def receive = {
    case CheckAllInput =>
      for (child <- context.children) child ! Player.CheckInput
    case NewPlayer(in, out, name) =>
      if(context.children.exists(_.path.name == name)) {
        out.println("That name is already tanken. Please choose a different name.")
        //sock.close()
      } else {
        context.actorOf(Props(new Player(name, location, inventory, in, out)), name)
        out.println("=> ")
      }
    case SendMessageToAll(message) =>
      for (child <- context.children) child ! Player.PrintMessage(message)
    case m => println("PlayerManager recieved unknown message: " + m)
  }
  
}

object PlayerManager{
  case object CheckAllInput
  case class NewPlayer(in: BufferedReader, out: PrintStream, name: String)
  case class SendMessageToAll(message:String)
}