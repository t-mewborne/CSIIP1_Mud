package mud

class Room (
    name:String,
    desc:String,
    private var items:List[Item],
    exits:Array[Int]){
  
  def getName(): String = name
  
  def description(): String = desc
    
  def getExit(dir: Int): Option[Room] = if(exits(dir) == -1) None else Some(Room.rooms(exits(dir)))
  
  def printExits(): String = {
    var possibleExits = "\nPossible Exit(s): "
    if (exits(0) != -1) possibleExits += "North, "
    if (exits(1) != -1) possibleExits += "South, "
    if (exits(2) != -1) possibleExits += "East, "
    if (exits(3) != -1) possibleExits += "West, "
    if (exits(4) != -1) possibleExits += "Up, "
    if (exits(5) != -1) possibleExits += "Down"
    
    var possibleExitsLength = possibleExits.length - 2 //The length of the string without the comma
    if (possibleExits(possibleExitsLength) == ',') possibleExits = possibleExits.substring(0, possibleExitsLength)
    
    possibleExits.trim
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
  
  def readRooms(): Map[String,Room] = { //TODO Fix all of the errors created by this line
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.trim.toInt)(readRoom(lines))
    source.close()
    rooms
  }
  
  def readRoom(lines: Iterator[String]):Room={
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next().trim.toInt){
      Item(lines.next,lines.next)
    }
    val exits = lines.next.split(",").map(_.trim.toInt)
    new Room(name,desc,items,exits)
  }
}