package mud

class Player (
    name:String,
    private var location:Int,
    private var inventory:List[Item])
    {
  
  def currentLocation (): Int = location
  
  def processCommand(command: String): Unit = {
    	if (command == "n" || command =="north") move("n")
	    if (command == "s" || command =="south") move("s")
	    if (command == "e" || command =="east") move("e")
	    if (command == "w" || command =="west") move("w")
	    if (command == "u" || command =="up") move("u")
	    if (command == "d" || command =="down") move("d")
	    else if (command == "look"){
	      println(Room.rooms(location).description)
	      //print items
	      //print exits
	    }
	    else if (command == "inv" || command == "inventory") println(inventoryListing())
	    //else If getItem (Create another input command to ask which item to get)
	    //else If dropItem (Create another input command to ask which item to drop)
	    else if (command == "help"){
	      println("\"n\" - Move North")
	      println("\"s\" - Move South")
	      println("\"e\" - Move East")
	      println("\"w\" - Move West")
	      println("\"u\" - Move Up")
	      println("\"d\" - Move Down")
	      println("\"look\" - Reprint the description of the room")
	      println("\"inv\" - Print what is currently in your inventory")
	      println("\"getItem\" - Allows you to pick up the specified item in a room and add it to your inventory")
	      println("\"dropItem\" - Allows you to drop a specified item from your inventory into the current room.")
	      println("\"help\" - Returns a list of possible commands.")
	      println("\"exit\" - Quit the game :(")
	    }
    		else println("Please enter a valid command. Type \"help\" for a list of valid commands.")
  }
  
  //Pull an item out of the inventory (if it exists) and return it to Room (then room will add it to the list of items in the room).
  def getFromInventory(itemName: String): Option[Item] = ???
  
  
  //Add an item to your inventory that you have picked up from your room.
  def addToInventory(item: Item): Unit = inventory:+ Room.rooms(location).getItem(item.name)
  
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
       println(Room.rooms(location).description) //Print the description of the new room
     }
    }
  }
}

/* Questions:
 * Not sure how to manipulate item lists Option[Item]
 */