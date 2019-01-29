package mud

class Player (
    name:String,
    private var location:Int,
    private var inventory:List[Item]){
  
  def currentLocation (): Int = location
  def processCommand(command: String): Unit = ???
  //Parse and act on a command
  
  def getFromInventory(itemName: String): Option[Item] = ???
  //Pull an item out of the inventory (if it exists) and return it.
  
  def addToInventory(item: Item): Unit = ???
  //Add the given item to inventory.
  
  //Build a String with the contents of the inventory for printing.
  def inventoryListing(): String = inventory.mkString(sep)
  
  //Move the player in a particular direction if possible. (n,s,e,w,u,d) If possible determined in main method.
  def move(dir: String): Unit = {
   if (dir =="n") location = Room.rooms(location).getNumber //This is not correct- .getNumber returns current room number
  }
}