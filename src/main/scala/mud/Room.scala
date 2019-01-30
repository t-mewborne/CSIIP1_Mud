package mud

class Room (
    number: Int,
    name:String,
    desc:String,
    private var items:List[Item],
    exits:Array[Int]){
  
  def getName(): String = name
  
  def description(): String = desc
  
  def getNumber(): Int = number
  
  def getExit(dir: Int): Option[Int] = if(exits(dir) == -1) None else Some(exits(dir))
  
  //Pick if an item (IF IT EXISTS) from a room. addToInventory (class Player) will use this to add the item to a player's inventory
  def getItem(itemName: String): Option[Item] = ???
  
  def dropItem(item: Item): Unit = ???

}

object Room {
  val rooms = readRooms()
  
  def readRooms(): Array[Room] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.trim.toInt)(readRoom(lines))
    source.close()
    rooms
  }
  
  def readRoom(lines: Iterator[String]):Room={
    val number = lines.next.trim.toInt
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next().trim.toInt){
      Item(lines.next,lines.next)
    }
    val exits = lines.next.split(",").map(_.trim.toInt)
    new Room(number,name,desc,items,exits) //I get the error: Next on empty iterator. If statement gives an error because it needs an else
  }
}