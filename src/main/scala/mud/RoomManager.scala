/*
 * TODO
 * Need to tell room manger where the player is initially
 */

package mud

import akka.actor.Actor
import akka.actor.ActorRef

class RoomManager extends Actor{
  val rooms = readRooms()
  for(room <- context.children) room ! Room.LinkExits(rooms)
  
    def receive = {
    case m => println("RoomManager recieved unknown message: " + m)
  }
  
  def readRooms(): Map[String,ActorRef] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val rooms = (lines.map(_.trim -> readRoom(lines))).toMap
    source.close()
    rooms
  }
  
  def readRoom(lines: Iterator[String]):ActorRef={ //TODO Should this return (String,ActorRef)?
    val name = lines.next
    val desc = lines.next
    val items = List.fill(lines.next().trim.toInt){
      Item(lines.next,lines.next)
    }
    val exits = lines.next.split(",").map(_.trim).toBuffer
    new Room(name,desc,items,exits)
  }
}