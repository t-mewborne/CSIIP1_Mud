package mud

import io.StdIn._

object Main {
	def main(args: Array[String]): Unit = {
	  var input = readLine("Hello! What is your name?: ").trim.capitalize
	  while (input == "") input = readLine("Please enter your name: ").trim.capitalize //Ask the user to enter their name if they do not enter their name
	  val player1 = new Player(input, Room.rooms("mbr"), List[Item](new Item("phone", "A device intended for communication, however typically used as a social deterrent.")))
	  println("Welcome " + input + "! Type \"help\" at any time for a list of possible commands.\n\nYou are currently in the " + player1.currentLocation.getName + ": " + player1.currentLocation.description + "\n")
	  println("Item(s) in your " + player1.inventoryListing + "\n") //Print the items in the player's inventory.
	  println(player1.currentLocation.itemList) //Print the items in the room
	  println("\n" + player1.currentLocation.printExits) //Print the possible exits
	  do {  
	    input = readLine("\n=> ").toLowerCase.trim
	    if (input == "quit" || input == "q" || input == "exit") input = "exit" //"e" cannot be used because "e" is used for east
	    else player1.processCommand(input)
	  } while (input != "exit") //Stop the loop when the user types q, quit, or exit
	    println("k.")
	}
}