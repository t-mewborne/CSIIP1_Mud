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
    case m => sender ! Player.PrintMessage("\n*****RoomManager received an unknown message: " + m + "*****")
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
      Item(lines.next, lines.next, lines.next.toInt, lines.next.toInt)
    }
    val exits = lines.next.split(",").map(_.trim)
    keyword -> context.actorOf(Props(new Room(name, desc, items, exits)), keyword)
  }

}

/*MAP FORMAT:
 * 1) Number of rooms
 * 2) Room Key
 * 3) Room Name
 * 4) Room Description
 * 5) Number of Items in the room
 * 6) Item 1 name
 * 7) Item 1 description
 * 8) Item 1 speed
 * 9) Item 1 damage
 * -- Repeat 6-9 for each item
 * 10)Room Exit Keys
 * -- Repeat 2-10 for each room
 */

object RoomManager {
  case class StartRoom(player:ActorRef)
}