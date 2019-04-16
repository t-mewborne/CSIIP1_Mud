package mud

import collection.mutable.Buffer
import akka.actor.Actor
import akka.actor.ActorRef

class Room (
    name:String,
    desc:String,
    private var items:List[Item],
    exitKeys: Array[String]) extends Actor{
  
  import Room._
  
  private var playerList: List[String] = Nil
  
  private var exits: Array[Option[ActorRef]] = null
  
  def receive = {
    case LinkExits(roomsMap)=> 
      exits = exitKeys.map(keyword => roomsMap.get(keyword))
    case GetDetails =>
      sender ! Player.PrintMessage(getName + "\n" + description + "\n" + itemList + "\n" + printExits + "\nPlayers: " + playerList.mkString(", ").capitalize + "\n")
    case GetExit(dir) =>
      sender ! Player.TakeExit(getExit(dir))
    case GetItem(itemName) =>
      sender ! Player.TakeItem(getItem(itemName))
    case DropItem(item) =>
      dropItem(item)
    case AddPlayer(name) =>
      playerList = name.capitalize :: playerList
    case RemovePlayer(name) =>
      playerList = playerList.filter(_ != name.capitalize)
    case m => sender ! Player.PrintMessage("Room recieved unknown message: " + m)
  }
  
  def getName(): String = name
  
  def description(): String = desc
    
  def getExit(dir: Int): Option[ActorRef] = exits(dir) //if(exits(dir) == "None") None else Some(ActorRef(exits(dir)))
  
  def printExits(): String = {
    var possibleExits = "\nPossible Exit(s): "
    if (exits(0) != None) possibleExits += "North, "
    if (exits(1) != None) possibleExits += "South, "
    if (exits(2) != None) possibleExits += "East, "
    if (exits(3) != None) possibleExits += "West, "
    if (exits(4) != None) possibleExits += "Up, "
    if (exits(5) != None) possibleExits += "Down  "
    
    if (possibleExits == "\nPossible Exit(s): ") "\nNo Exits :)" 
      else possibleExits.substring(0, possibleExits.length-2).trim
  }
  
  //Pick if an item (IF IT EXISTS) from a room. addToInventory (class Player) will use this to add the item to a player's inventory
  def getItem(itemName: String): Option[Item] = {
    val item = items.find(_.name == itemName)
    item match {
      case(None) => None
      case(Some(i)) => items = items.dropWhile(_.name == itemName)
    }
    item
  }
  
  def dropItem(item: Item): Unit = items = item :: items
  
  def itemList():String={
    if (items.length > 0) "Item(s) in room:\n"+ items.map(item => "\t" + item.name.capitalize + " - " + item.desc).mkString("\n")
    else "Item(s) in room: None"
  }

}

object Room {
  case class LinkExits(roomsMap: Map[String,ActorRef])
  case object GetDetails
  case class GetExit(dir: Int)
  case class GetItem(itemName:String)
  case class DropItem(item:Item)
  case class AddPlayer(name:String)
  case class RemovePlayer(name:String)
}