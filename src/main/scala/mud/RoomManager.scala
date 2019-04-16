package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.util.Random

class RoomManager extends Actor {
  var keywords:List[String]=List()
  val rand = new Random
	val rooms = readRooms()
	for(room <- context.children) room ! Room.LinkExits(rooms)
	import RoomManager._
	
  def receive = {
	  case StartRoom(entity) =>
	    entity ! Player.StartingRoom(rooms(keywords(rand.nextInt(keywords.length))))
    case m => sender ! Player.PrintMessage("RoomManager recieved unknown message: " + m)
  }

  def readRooms(): Map[String, ActorRef] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val rooms = Array.fill(lines.next.trim.toInt)(readRoom(lines)).toMap
    source.close()
    rooms
  }

  def readRoom(lines: Iterator[String]): (String, ActorRef) = {
    val keyword = lines.next
    keywords = keyword :: keywords
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next.trim.toInt) {
      Item(lines.next, lines.next)
    }
    val exits = lines.next.split(",").map(_.trim)
    keyword -> context.actorOf(Props(new Room(name, desc, items, exits)), keyword)
  }

}

object RoomManager {
  case class StartRoom(player:ActorRef)
}