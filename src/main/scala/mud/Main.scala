package mud

import io.StdIn._

object Main {
	def main(args: Array[String]): Unit = {
	  var input = readLine("Hello! What is your name?: ").trim
	  val player1 = new Player(input, 0, List[Item](new Item("iPhone", "A device intended for communication, however typically used as a social deterrent")))
	  print("Hello " + input + "! Welcome to ___. Type \"help\" at any time for a list of possible commands.\nYou are currently in " + Room.rooms(player1.currentLocation).getName)
	  println(Room.rooms(player1.currentLocation).description)
	  //Print Exit(s)
	  //Print item(s)
	  do {  
	    input = readLine("=> ").toLowerCase.trim
	    if (input == "quit" || input == "q" || input == "exit" || input == "e") input = "exit"
	    else player1.processCommand(input)
	  } while (input != "exit")
	    println("k.")
	}
}