package mud

import akka.actor.Actor
import akka.actor.ActorRef
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import akka.actor.Kill

class Player (
    name:String,
    in: BufferedReader, 
    out: PrintStream,
    sock: Socket)
    extends Actor{

  import Player._

	private var location:ActorRef = null
	private var inventory:List[Item] = List()


  def receive = {
    case CheckInput =>
      if (in.ready()) {
        val input = in.readLine().trim.toLowerCase
        processCommand(input)
        //out.print("=> ")
      }
    case PrintMessage(message) =>
      out.println(message)
      out.print("=> ")
    case PrintMessageRoom(message, room)=>
      if (location == room) out.print("\n\n" + message + "\n\n=>")
    case TakeExit(optRoom) => //TODO
      if (optRoom == None) out.println("\nYou cannot go this way.") else {
        location = optRoom.get
        out.println()
        location ! Room.GetDetails
      }
    case TakeItem(optItem) =>
      optItem match {
        case None => out.println("I do not see that item in the room.") //List current room item list
        case Some(i) =>
          addToInventory(i)
          out.println("Picked up " + i.name + "\n\n=>")
      }
    case StartingRoom(room) =>
      location = room
      out.println("Welcome " + name.capitalize + "!")
      location ! Room.GetDetails
    case m => println("Player recieved unknown message: " + m)
  }
  
  //def currentLocation():Room = location
    
  def processCommand(command: String): Unit = {
    	if (command == "n" || command =="north") location ! Room.GetExit(0)
	    else if (command == "s" || command =="south") location ! Room.GetExit(1)
	    else if (command == "e" || command =="east") location ! Room.GetExit(2)
	    else if (command == "w" || command =="west") location ! Room.GetExit(3)
	    else if (command == "u" || command =="up") location ! Room.GetExit(4)
	    else if (command == "d" || command =="down") location ! Room.GetExit(5)
	    else if (command == "look") location ! Room.GetDetails
	    else if (command == "inv" || command == "inventory") out.print(inventoryListing() +  "\n\n=>")
	    else if (command.startsWith("get") && command(3) == ' '){
	     var getSplit = command.split(" ") //Split get command at the space
	     location ! Room.GetItem(getSplit(1))
	      out.print("\n\n=>")
	    }
    	else if (command.startsWith("drop") && command(4) == ' '){
	      var dropSplit = command.split(" ") //Split drop coyell (message)"--------------- Send a message to all players (does not work yet)mmand at the space
	      var itemToDrop = getFromInventory(dropSplit(1))
	      itemToDrop match {
	        case None => out.println("I could not find that item in your " + inventoryListing)
	        case Some(i) => {
	          getFromInventory(i.name)
	          location ! Room.DropItem(i)
	          out.println("Dropped " + dropSplit(1))
	        }
	        out.print("\n\n=>")
	      }
	    }
    	else if (command.startsWith("say") && command(3) == ' ') {
    	  var saySplit = command.split(" ")
    	  context.parent ! PlayerManager.TellRoom(name.capitalize+" said \""+saySplit.drop(1).mkString(" ") + "\"", location)
    	}
    	else if (command.startsWith("tell") && command(4) == ' ') {
    	  var tellSplit = command.split(" ")
    	  context.parent ! PlayerManager.TellPlayer(name, tellSplit(1).toLowerCase, tellSplit.drop(2).mkString(" "))
    	}
    	else if (command == "players") out.println(command + " is not a valid command yet.")
    	else if (command == "exit") {
    	  out.println("\nGoodbye!\n")
    	  sock.close()
    	  out.close()
    	  in.close()
    	  context.stop(self) //Kill the actor so people cannot send messages to dead actors
    	  //TODO Add all items of this player into the exit room
    	}
	    else if (command == "help"){
	      out.println("\n\"n\" ------------------------- Move North")
	      out.println("\"s\" --------------------------- Move South")
	      out.println("\"e\" --------------------------- Move East")
	      out.println("\"w\" --------------------------- Move West")
	      out.println("\"u\" --------------------------- Move Up")
	      out.println("\"d\" --------------------------- Move Down")
	      out.println("\"look\" ------------------------ Reprint the description, items, and possible exits of the current room")
	      out.println("\"inv\" ------------------------- Print what is currently in your inventory")
	      out.println("\"get (item)\" ------------------ Pick up a specified item in a room and add it to your inventory")
	      out.println("\"drop (item)\" ----------------- Drop a specified item from your inventory into the current room.")
	      out.println("\"say (message)\"---------------- Send a message to all players")
	      out.println("\"tell (player) (message)\" ----- Send a message to a specific player")
	      out.println("\"players\"---------------------- Print players in game and their locations (does not work yet)") //TODO
	      out.println("\"help\" ------------------------ A list of possible commands")
	      out.println("\"exit\" ------------------------ Quit the game :(")
	      out.print("\n=>")
	    }
    	else {
    	  out.println("\nI don't know what \""+command+"\" means. Please enter a valid command. (Type \"help\" for a list of valid commands)")
    	  out.print("\n=>")
    	}
  }
  
  //Pull an item out of the inventory and return if that item exists
  def getFromInventory(itemName: String): Option[Item] = {
    val item = inventory.find(_.name == itemName)
    item match {
      case(None) => None
      case(Some(i)) => inventory = inventory.dropWhile(_.name == itemName)
    }
    item
  }
  
  
  //Add an item to your inventory that you have picked up from your room.
  def addToInventory(item: Item): Unit = inventory = item :: inventory
  
  //Build a String with the contents of the inventory for printing.
  def inventoryListing(): String = {
    if (inventory.length > 0) "Inventory:\n"+ inventory.map(item => "\t" + item.name.capitalize + " - " + item.desc).mkString("\n")
    else "Inventory: Empty"
  }
}

object Player{
  case class PrintMessage(message:String)
  case class TakeExit(optRoom:Option[ActorRef])
  case class TakeItem(optItem:Option[Item])
  case object CheckInput
  case class StartingRoom(room:ActorRef)
  case class PrintMessageRoom(message:String,room:ActorRef)
}
/* Format of the map.txt file:
 * 1 Room 1 Key
 * 2 Room 1 Name
 * 3 Room 1 Description
 * 4 Room 1 Quantity of items
 * 5 Room 1 Item 1 Name
 * 6 Room 1 Item 1 Description [Repeat 5 and 6 for every item]
 * 7 Room 1 Possible Exits (n,s,e,w,u,d) None indicates there is not an exit, anything else indicates the key of the room in that direction
 * Repeat 2-7 for each room
 */