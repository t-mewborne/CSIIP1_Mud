package mud

import collection.mutable.Buffer

class Room (
    name:String,
    desc:String,
    private var items:List[Item],
    exits: Buffer[String]) {
  
  def getName(): String = name
  
  def description(): String = desc
    
  def getExit(dir: Int): Option[Room] = if(exits(dir) == "None") None else Some(Room.rooms(exits(dir)))
  
  def printExits(): String = {
    var possibleExits = "\nPossible Exit(s): "
    if (exits(0) != "None") possibleExits += "North, "
    if (exits(1) != "None") possibleExits += "South, "
    if (exits(2) != "None") possibleExits += "East, "
    if (exits(3) != "None") possibleExits += "West, "
    if (exits(4) != "None") possibleExits += "Up, "
    if (exits(5) != "None") possibleExits += "Down  "
    
    possibleExits.substring(0, possibleExits.length-2).trim
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
  val rooms = readRooms()
  
  def readRooms(): Map[String,Room] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val rooms = (lines.map(_.trim -> readRoom(lines))).toMap
    source.close()
    rooms
  }
  
  def readRoom(lines: Iterator[String]):Room={
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next().trim.toInt){
      Item(lines.next,lines.next)
    }
    val exits = lines.next.split(",").map(_.trim).toBuffer
    //val uselessVal = lines.next() //TODO Find a better way of doing this, this allows me to space out each room in the map.txt file
    new Room(name,desc,items,exits)
  }
}