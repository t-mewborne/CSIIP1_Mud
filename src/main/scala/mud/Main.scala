package mud

import akka.actor.ActorSystem
import akka.actor.Props
import java.net.ServerSocket
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.PrintStream
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Main {
	def main(args: Array[String]): Unit = {
	  val system = ActorSystem("MudController")
    val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
	  system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, PlayerManager.CheckAllInput)

	  //val ss = new ServerSocket(8080)
	  
    while (true) {
      //val sock = ss.accept()
      val in = new BufferedReader(new InputStreamReader(Console.in))/*sock.getInputStream*/
      val out = new PrintStream(Console.out) /*sock.getOutputStream*/
      Future {
        out.println("What is your name?")
        val name = in.readLine().trim.capitalize
        playerManager ! PlayerManager.NewPlayer(in, out, name)
      }
    }
	  
	  //var input = readLine("Hello! What is your name?: ").trim.capitalize
	  //while (input == "") input = readLine("Please enter your name: ").trim.capitalize //Ask the user to enter their name if they do not enter their name
	  //val player1 = new Player(input, Room.rooms("mbr"), List[Item](new Item("phone", "A device intended for communication, however typically used as a social deterrent.")))
	  //println("Welcome " + input + "! Type \"help\" at any time for a list of possible commands.\n\nYou are currently in the " + player1.currentLocation.getName + ": " + player1.currentLocation.description + "\n")
	  //println("Item(s) in your " + player1.inventoryListing + "\n") //Print the items in the player's inventory.
	  //println(player1.currentLocation.itemList) //Print the items in the room
	  //println("\n" + player1.currentLocation.printExits) //Print the possible exits
	  //do {  
	  //  input = readLine("\n=> ").toLowerCase.trim
	  //  if (input == "quit" || input == "q" || input == "exit") input = "exit" //"e" cannot be used because "e" is used for east
	  //  else player1.processCommand(input)
	  //} while (input != "exit") //Stop the loop when the user types q, quit, or exit
	  //  println("k.")
	}
}