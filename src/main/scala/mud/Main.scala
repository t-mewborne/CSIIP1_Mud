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
  system.scheduler.schedule(0.seconds, 0.1.seconds, playerManager, PlayerManager.CheckAllInput)

  val portNumber = 8080
  val ss = new ServerSocket(portNumber)
  println("Server active on port " + portNumber)

  while (true) {
    val sock = ss.accept()
    val in = new BufferedReader(new InputStreamReader(sock.getInputStream))
    val out = new PrintStream(sock.getOutputStream)
    Future {
      out.print("What is your name?\n\n=>")
      val name = in.readLine().trim.toLowerCase
      playerManager ! PlayerManager.NewPlayer(in, out, name, sock)
    }
  }
}