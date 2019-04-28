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
import scala.io._

object Main extends App {
  val system = ActorSystem("MudController")
  val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
  val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
  val npcManager = system.actorOf(Props[NPCManager], "NPCManager")
  val activityManager = system.actorOf(Props[ActivityManager], "ActivityManager")
  system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, PlayerManager.CheckAllInput)
  system.scheduler.schedule(0.seconds, 0.1.seconds, activityManager, ActivityManager.CheckQueue)
  
  val NPCNames = Array("quinn",    "travis", "morgan",   "freddie",   "lauren", "naudia",    "ryanna",  "kenna", "bela",  "mark", "ghost", "bygoe")
  val NPCItems = Array("Uniwhale", "Knife",  "Computer", "Sunscreen", "Salt",   "Gift Card", "Hammock", "Hmm",   "Mario", "Hair", "Brick", "Frito")
  val NPCItemSpecs = Array((1,6),  (1,7),    (9,9),      (9,1),       (1,9),    (1,8),       (3,3),     (6,7),   (1,6),   (1,0),  (2,10),  (9,7)) //(Speed, Damage), ((0,10],[0,10])
  for (i <- 0 until NPCNames.length) npcManager ! NPCManager.newNPC(NPCNames(i),NPCItems(i),NPCItemSpecs(i))

  val portNumber = 8181
  val ss = new ServerSocket(portNumber)
  println("Server active on port " + portNumber)


  while (true) {
    val sock = ss.accept()
    val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
    val out = new PrintStream(sock.getOutputStream)
    Future {
      out.print("What is your name?\n\n=>")
      val name = in.readLine().trim.toLowerCase
      if (!name.forall(x => x.isLetterOrDigit)) {
        out.println("\nSorry, your name contains an illegal character. Please try again. \n")
        in.close()
        out.close()
        sock.close()
      } 
      else if (NPCNames.contains(name)) {
        out.println("\nSorry, that name is taken by an NPC. Please try again. \n")
        in.close()
        out.close()
        sock.close()
      }
      else playerManager ! PlayerManager.NewPlayer(in, out, name, sock)
    }
  }
}