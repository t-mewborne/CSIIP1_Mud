package mud

import akka.actor.Actor
import akka.actor.ActorRef
import java.io.PrintStream
import java.io.BufferedReader

class Player (
    name:String,
    private var location:Room,
    private var inventory:List[Item],
    in: BufferedReader, 
    out: PrintStream)
    extends Actor{

  import Player._

  def receive = {
    case CheckInput =>
      if (in.ready()) {
        val input = in.readLine()
        processCommand(input)
      }
    case PrintMessage(message) =>
      out.println(message)
    case TakeExit(optRoom) => //TODO
    case TakeItem(optItem) => getFromInventory(optItem)
    case m => println("Player recieved unknown message: " + m)
  }
  
  def currentLocation():Room = location
    
  def processCommand(command: String): Unit = {
    	if (command == "n" || command =="north") if (location.getExit(0) == None) println("\nYou cannot go North.") else move("n")
	    else if (command == "s" || command =="south") if (location.getExit(1) == None) println("\nYou cannot go South.") else move("s")
	    else if (command == "e" || command =="east") if (location.getExit(2) == None) println("\nYou cannot go East.") else move("e")
	    else if (command == "w" || command =="west") if (location.getExit(3) == None) println("\nYou cannot go West.") else move("w")
	    else if (command == "u" || command =="up") if (location.getExit(4) == None) println("\nYou cannot go Up.") else move("u")
	    else if (command == "d" || command =="down") if (location.getExit(5) == None) println("\nYou cannot go Down.") else move("d")
	    else if (command == "look"){
	      println(location.getName+"\n")
	      println(location.description +"\n")
  	    println(location.itemList +"\n")
	      println(location.printExits +"\n")
	    }
	    else if (command == "inv" || command == "inventory") println(inventoryListing())
	    else if (command.startsWith("get") && command(3) == " "){
	      var getSplit = command.split(" ") //Split get command at the space
	      var itemToGet = location.getItem(getSplit(1))
	      itemToGet match {
	        case None => println("I do not see that item in the room. Current " + location.itemList) //List current room item list
	        case Some(i) => addToInventory(i)
	        println("Picked up " + getSplit(1))
	      }
	    }
    	else if (command.startsWith("drop") && command(4) == " "){
	      var dropSplit = command.split(" ") //Split drop command at the space
	      var itemToDrop = getFromInventory(dropSplit(1))
	      itemToDrop match {
	        case None => println("I could not find that item in your " + inventoryListing)
	        case Some(i) => {
	          getFromInventory(itemToDrop.toString)
	          location.dropItem(i)
	          println("Dropped " + dropSplit(1))
	        }
	      }
	    }
    	else if (command.startsWith("yell") && command(4) == " ") { //TODO possible bug- this will still work with commands w/o spaces (ie yellow)
    	  var yellSplit = command.split(" ")
    	  context.parent ! PlayerManager.SendMessageToAll(name+" said "+yellSplit(1))
    	}
    	else if (command.startsWith("whisper")) println(command + " is not a valid command yet.")
    	else if (command == "players") println(command + " is not a valid command yet.")
    	else if (command == "exit") println("Command Unavailable.")
	    else if (command == "help"){
	      println("\n\"n\" ------------------------- Move North")
	      println("\"s\" --------------------------- Move South")
	      println("\"e\" --------------------------- Move East")
	      println("\"w\" --------------------------- Move West")
	      println("\"u\" --------------------------- Move Up")
	      println("\"d\" --------------------------- Move Down")
	      println("\"look\" ------------------------ Reprint the description, items, and possible exits of the current room")
	      println("\"inv\" ------------------------- Print what is currently in your inventory")
	      println("\"get (item)\" ------------------ Pick up a specified item in a room and add it to your inventory")
	      println("\"drop (item)\" ----------------- Drop a specified item from your inventory into the current room.")
	      println("\"yell (message)\"--------------- Send a message to all players (does not work yet)") //TODO
	      println("\"whisper (player) (message)\" -- Send a message to a specific player (does not work yet)") //TODO
	      println("\"players\"---------------------- Print players in game and their locations (does not work yet)") //TODO
	      println("\"help\" ------------------------ A list of possible commands")
	      println("\"exit\" ------------------------ Quit the game :(")
	    }
    	else println("\nI don't know what \""+command+"\" means. Please enter a valid command. (Type \"help\" for a list of valid commands)")
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

  
  //Move the player in a particular direction. (n[0],s[1],e[2],w[3],u[4],d[5])
  def move(dir: String): Unit = {
   if (dir =="n") location = location.getExit(0).get
   if (dir =="s") location = location.getExit(1).get
   if (dir =="e") location = location.getExit(2).get
   if (dir =="w") location = location.getExit(3).get
   if (dir =="u") location = location.getExit(4).get
   if (dir =="d") location = location.getExit(5).get
   println("\n" + location.getName) //Print the name of the room
   println(location.description) //Print the description of the new room
   println("\n" + location.itemList) //Print the items currently in the new rooms
   println("\n" + location.printExits) //Print the possible exits
  }
}

object Player{
  case class PrintMessage(message:String)
  case class TakeExit(optRoom:Option[ActorRef])
  case class TakeItem(optItem:Option[Item])
  case object CheckInput
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