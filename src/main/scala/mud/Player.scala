package mud

class Player (
    name:String,
    private var location:Int,
    private var inventory:List[Item])
    {
  
  def currentLocation (): Int = location
  def processCommand(command: String): Unit = ???
  
  def getFromInventory(itemName: String): Option[Item] = ???
  //Pull an item out of the inventory (if it exists) and return it.
  
  def addToInventory(item: Item): Unit = ???
  //Add the given item to inventory.
  
  //Build a String with the contents of the inventory for printing.
  def inventoryListing(): String = {
    "Inventory:\n"+
    inventory.map(item => item.name + " - " + item.desc).mkString("\n")
  }

  
  //Move the player in a particular direction if possible. (n,s,e,w,u,d) If possible determined in main method.
  def move(dir: String): Unit = {
   if (dir =="n") Room.rooms(location).getExit(0) match {
     case None => println("You cannot go that way.")
     case Some(roomNumber) => {
       location = roomNumber
       println(Room.rooms(location).description)
     }
    }
  }
}

/* Questions:
 * Player line 23 -- shouldn't I use .getExit? How? How do I get an Int from Option[Room]?
 * What is processCommand (player line 9)
 * Confused on how to manipulate item lists Option[Item]
 */