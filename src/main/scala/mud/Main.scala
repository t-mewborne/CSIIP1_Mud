package mud

import io.StdIn._
object Main {
	def main(args: Array[String]): Unit = {
	  val playerName = readLine("Hello! What is your name?: ").trim
	  val player1 = new Player(playerName, 0, List[Item](new Item("iPhone", "A device intended for communication, however typically used as a social deterrent")))
	  print("Hello ",playerName,"! Welcome to ___. Type \"help\" at any time for a list of possible commands.\nYou are currently in ")
	  var input = ""
	  do {
	    println(Room.rooms(player1.currentLocation).getName)
	    println(Room.rooms(player1.currentLocation).description)
	    //Print Exit(s)
	    //Print item(s)
	    
	    input = readLine("=> ").toLowerCase.trim
	    
	    if (input == "n" || input =="north") {
	      if (Room.rooms(player1.currentLocation).getExit(0) != None) player1.move("n")
	      else println("You cannot go that way.") //List of valid exits?
	    }
	    //else If s
	    //else If e
	    //else If w
	    //else If up
	    //else If down
	    else if (input == "look"){
	      println(Room.rooms(player1.currentLocation).description)
	      //print items
	      //print exits
	    }
	    else if (input == "inv" || input == "inventory") println(player1.inventoryListing())
	    //else If getItem
	    //else If dropItem
	    else if (input == "help"){
	      println("\"n\" - Move North")
	      println("\"s\" - Move South")
	      println("\"e\" - Move East")
	      println("\"w\" - Move West")
	      println("\"u\" - Move Up")
	      println("\"d\" - Move Down")
	      println("\"look\" - Print the description of the room")
	      println("\"inv\" - Print what is currently in your inventory")
	      println("\"get (item)\" - Allows you to pick up the specified item in a room and add it to your inventory")
	      println("\"drom (item)\" - Allows you to drop a specified item from your inventory into the current room.")
	      println("\"help\" - Returns a list of possible commands.")
	      println("\"exit\" - Type this to quit the game :(")
	    }
	    else if (input == "quit" || input == "q" || input == "exit") input = "exit"
	    else println("Please enter a valid command. Type \"help\" for a list of valid commands.")
	    
	  } while (input != "exit")
	    println("k.")
	}
}
