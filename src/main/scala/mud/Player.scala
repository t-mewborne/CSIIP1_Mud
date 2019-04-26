package mud

import akka.actor.Actor
import akka.actor.ActorRef
import java.io.PrintStream
import java.io.BufferedReader
import java.net.Socket
import akka.actor.Kill
import scala.util.Random

class Player(
  name: String,
  in: BufferedReader,
  out: PrintStream,
  sock: Socket)
  extends Actor {

  import Player._

  private var location: ActorRef = null
  private var victim: ActorRef = null
  private var inventory: DoublyLinkedList[Item] = new DoublyLinkedList()
  private var equippedItem: Option[Item] = None
  private var itemSpeed = 9
  private var itemDamage = 3
  private var itemName = "Fist"
  private var health = 100
  private var combatMode = false

  val rand = new Random

  def receive = {
    case CheckInput =>
      if (in.ready()) {
        val input = in.readLine().trim.toLowerCase
        processCommand(input)
      }
    case PrintMessage(message) =>
      out.println(message)
      out.print("=>")
    case PrintMessageRoom(message, room) =>
      if (location == room) out.print("\n\n" + message + "\n\n=>")
    case TakeExit(optRoom) =>
      if (optRoom == None) out.print("\nYou cannot go this way.\n\n=>")
      else {
        location ! Room.RemovePlayer
        context.parent ! PlayerManager.TellRoom(name.capitalize + " has left the room.", location)
        location = optRoom.get
        out.println()
        location ! Room.AddPlayer
        context.parent ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
        location ! Room.GetDetails
      }
    case TakeItem(optItem) =>
      optItem match {
        case None => out.print("\nI do not see that item in the room.\n\n=>") //List current room item list
        case Some(i) =>
          addToInventory(i)
          out.print("\nPicked up \"" + i.name.capitalize + "\"\n\n=>")
          context.parent ! PlayerManager.TellRoom(name.capitalize + " picked up \"" + i.name.capitalize + "\"", location)
      }
    case StartingRoom(room) =>
      location = room
      location ! Room.AddPlayer
      out.println("Welcome " + name.capitalize + "!")
      context.parent ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
      location ! Room.GetDetails
    case PlayerFound(optPlayer) =>
      optPlayer match {
        case Some(i) =>
          i ! Attacked(self)
          victim = i
          combatMode = true
          Main.activityManager ! ActivityManager.Enqueue(HitYou, rand.nextInt(itemSpeed))
        case None => out.print("\nThat player is not in the room.\n\n=>")
      }
    case HitYou =>
      if (combatMode) {
        
        var damage = rand.nextInt(itemDamage + 1)
        victim ! HitMe(self, damage, itemName)
        out.print("\n\nYou dealt " + damage + " damage to " + name.capitalize + " with \"" + itemName + "\".\nOpponent's health: " + health + "\n\n=>")
      }
    case HitMe(playerThatHit, damage, item) =>
      if (combatMode) {
        health -= damage
        if (health < 0) health = 0
        out.print("\n\n" + playerThatHit.path.name.capitalize + " dealt " + itemDamage + " damage to you with \"" + item + "\".\nYour health: " + health + "\n\n=>")
        Main.activityManager ! ActivityManager.Enqueue(HitYou, rand.nextInt(itemSpeed))
      } else playerThatHit ! PrintMessage("\nPlayer not in combat mode.")
      if (health <= 0) {
        out.print("\n\n" + playerThatHit.path.name.capitalize + " killed you.\n\n")
        sender ! OtherPlayerKilled(name.capitalize)
        removePlayerFromGame
      }
    case OtherPlayerKilled(opponentName) =>
      out.print("\n\nYou killed " + opponentName + "!\n\n=>")
      context.parent ! PlayerManager.TellRoom(name.capitalize + " " + "killed " + opponentName, location)
      combatMode = false
    case OpponentFled =>
      out.print("\n\nYour opponent has fled.\n\n=>")
      combatMode = false
    case Attacked(otherPlayer) =>
      if (!combatMode){
        victim = otherPlayer
        combatMode = true
        out.print("\n\n"+otherPlayer.path.name.capitalize + " is attacking you!\n\n=>")
      } else otherPlayer ! PlayerBusy
    case PlayerBusy =>
        out.println("That player is busy\n\n=>")
        combatMode = false
    case TakeFlee(optRoom) =>
        if (optRoom == None) out.print("\nFlee Attempt Failed\n\n=>")
        else {
        location ! Room.RemovePlayer
        context.parent ! PlayerManager.TellRoom(name.capitalize + " fled battle.", location)
        location = optRoom.get
        out.println("\nFlee Success!\n")
        location ! Room.AddPlayer
        context.parent ! PlayerManager.TellRoom(name.capitalize + " has entered the room.", location)
        location ! Room.GetDetails
        }
    case m => out.println("Player recieved unknown message: " + m)
  }

  //def currentLocation():Room = location

  def processCommand(command: String): Unit = {
    val notAllowed = List("n", "s", "e", "w", "u", "d", "north", "south", "east", "west", "up", "down", "get", "drop", "kill", "exit")

    if ((command == "n" || command == "north") && !combatMode) location ! Room.GetExit(0)
    else if ((command == "s" || command == "south") && !combatMode) location ! Room.GetExit(1)
    else if ((command == "e" || command == "east") && !combatMode) location ! Room.GetExit(2)
    else if ((command == "w" || command == "west") && !combatMode) location ! Room.GetExit(3)
    else if ((command == "u" || command == "up") && !combatMode) location ! Room.GetExit(4)
    else if ((command == "d" || command == "down") && !combatMode) location ! Room.GetExit(5)

    else if (command == "look") {
      out.println()
      location ! Room.GetDetails
    } else if (command.startsWith("get") && command(3) == ' ' && command.length > 4 && !combatMode) {
      var getSplit = command.split(" ")
      location ! Room.GetItem(getSplit.drop(1).mkString(" "))
    } else if (command.startsWith("drop") && command(4) == ' ' && command.length > 4 && !combatMode) {
      var dropSplit = command.split(" ")
      var itemName = dropSplit.drop(1).mkString(" ")
      var itemToDrop = getFromInventory(itemName)
      itemToDrop match {
        case None => out.print("\nI could not find that item in your " + inventoryListing + "\n\n=>")
        case Some(i) =>
          {
            getFromInventory(i.name)
            location ! Room.DropItem(i)
            out.print("\nDropped \"" + itemName.capitalize + "\"")
            context.parent ! PlayerManager.TellRoom(name.capitalize + " dropped \"" + i.name.capitalize + "\"", location)
          }
          out.print("\n\n=>")
      }
    } else if (command.startsWith("kill") && command(4) == ' ' && !combatMode) {
      var killSplit = command.split(" ")
      location ! Room.FindPlayer(killSplit.drop(1).mkString(" "))
    } else if (command == "inv" || command == "inventory") out.print("\n" + inventoryListing() + "\n\n=>")

    else if (command.startsWith("equip") && command(5) == ' ' && command.length > 5) {
      var equipSplit = command.split(" ")
      var itemToEquip = getFromInventory(equipSplit.drop(1).mkString(" "))
      itemToEquip match {
        case None => out.print("\nI could not find that item in your " + inventoryListing + "\n\n=>")
        case Some(i) =>
          if (equippedItem == None) {
            equippedItem = Some(i)
          } else {
            addToInventory(equippedItem.get)
            equippedItem = Some(i)
          }
          itemSpeed = i.speed
          itemDamage = i.damage
          itemName = i.name
          out.print("\nEquipped \"" + i.name + "\"\n\n=>")
      }
    } else if (command == "unequip") {
      equippedItem match {
        case Some(i) =>
          addToInventory(i)
          out.print("\nUnequipped \"" + equippedItem.get.name + "\"\n\n=>")
          equippedItem = None
          itemSpeed = 9
          itemDamage = 1
          itemName = "Fist"
        case None => out.print("\nYou don't have an equipped item.\n\n=>")
      }
    } else if (command.startsWith("say") && command(3) == ' ') {
      var saySplit = command.split(" ")
      context.parent ! PlayerManager.TellRoom(name.capitalize + " said \"" + saySplit.drop(1).mkString(" ") + "\"", location)
      out.println("\nYou said \"" + saySplit.drop(1).mkString(" ") + "\"")
      out.print("\n=> ")
    } else if (command.startsWith("tell") && command(4) == ' ') {
      var tellSplit = command.split(" ")
      context.parent ! PlayerManager.TellPlayer(name, tellSplit(1).toLowerCase, tellSplit.drop(2).mkString(" "))
    } else if (command == "players") {
      context.parent ! PlayerManager.PrintPlayers
      Main.npcManager ! NPCManager.PrintNPC
    } else if (command == "exit" && !combatMode) {
      out.println("\nGoodbye!\n")
      removePlayerFromGame
    } else if (command == "flee" && combatMode) {
      combatMode = false
      location ! Room.FleeAttempt(rand.nextInt(5))
      victim ! OpponentFled
      out.print("\nYou have fled.\n\n=>")
    } else if (command == "health"){
      out.print("\nHealth: " + health + "\n\n=>")
    } else if (command == "help") {
      out.println()
      if (!combatMode) {
        out.println("\"n\" --------------------------- Move North")
        out.println("\"s\" --------------------------- Move South")
        out.println("\"e\" --------------------------- Move East")
        out.println("\"w\" --------------------------- Move West")
        out.println("\"u\" --------------------------- Move Up")
        out.println("\"d\" --------------------------- Move Down")
        out.println("\"get <item>\" ------------------ Pick up a specified item in a room and add it to your inventory")
        out.println("\"drop <item>\" ----------------- Drop a specified item from your inventory into the current room.")
        out.println("\"kill <player>\" --------------- Attack a specified player")
      } else {
        out.println("flee----------------------------- Exit the battle")
      }
      out.println("\"look\" ------------------------ Reprint the description, items, possible exits, and players in the current room")
      out.println("\"inv\" ------------------------- Print what is currently in your inventory")
      out.println("\"equip <item>\"----------------- Equip a specified item from your inventory for battle")
      out.println("\"unequip\"---------------------- Unequip an item (if equipped) for battle")
      out.println("\"say <message>\"---------------- Send a message to all players in the current room")
      out.println("\"tell <player> <message>\" ----- Send a message to a specific player")
      out.println("\"players\"---------------------- Print players and NPCs in the game")
      out.println("\"help\" ------------------------ A list of possible commands")
      out.println("\"health\" ---------------------- Print your current health.")
      out.println("\"exit\" ------------------------ Quit the game :(")
      out.print("\n=>")
    } else if (notAllowed.contains(command)) {
      out.print("\nYou cannot do that while in combat mode.\n\n=>")
    } else {
      out.print("\nI don't know what \"" + command + "\" means. Please enter a valid command. (Type \"help\" for a list of valid commands)\n\n=>")
    }
  }

  //Pull an item out of the inventory and return if that item exists
  def getFromInventory(itemName: String): Option[Item] = {
    val item = inventory.find(_.name == itemName)
    item match {
      case (None) => None
      case (Some(i)) =>
        inventory.remove(i)
    }
    item
  }

  //Add an item to your inventory that you have picked up from your room.
  //def addToInventory(item: Item): Unit = inventory = item :: inventory
  def addToInventory(item: Item): Unit = inventory += item

  //Build a String with the contents of the inventory for printing.
  def inventoryListing(): String = {
    if (inventory.length > 0) "Inventory:\n" + inventory.map(item => "\t" + item.name.capitalize + " - " + item.desc + "\n\t\tSpeed: " + item.speed + " Damage: " + item.damage).mkString("\n")
    else "Inventory: Empty"
  }

  def removePlayerFromGame: Unit = {
    //TODO Add all items of this player into the exit room
    location ! Room.RemovePlayer //Remove the player from the room so their ghost does not remain
    context.parent ! PlayerManager.TellRoom(name.capitalize + " has left the game.", location)
    sock.close()
    out.close()
    in.close()
    context.stop(self) //Kill the actor so people cannot send messages to dead actors
  }
}

object Player {
  case class PrintMessage(message: String)
  case class TakeExit(optRoom: Option[ActorRef])
  case class TakeFlee(optRoom: Option[ActorRef])
  case class TakeItem(optItem: Option[Item])
  case object CheckInput
  case class StartingRoom(room: ActorRef)
  case class PrintMessageRoom(message: String, room: ActorRef)
  case class PlayerFound(optPlayer: Option[ActorRef])
  case class Attacked(otherPlayer: ActorRef)
  case class TakeVictim(room: ActorRef)
  case object HitYou
  case class HitMe(playerThatHit: ActorRef, damage: Int, item: String)
  case object OpponentFled
  case class OtherPlayerKilled(opponentName:String)
  case object PlayerBusy
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