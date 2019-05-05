package mud

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

class RoomManager extends Actor {
  var keywords: List[String] = List()
  var names: List[String] = List()
  val rand = new Random
  private var connect: Map[String, Array[String]] = Map()
  val rooms = readRooms()
  
  for (room <- context.children) room ! Room.LinkExits(rooms)
  import RoomManager._

  def receive = {
    case StartRoom(entity) =>
      entity ! Player.StartingRoom(rooms(keywords(rand.nextInt(keywords.length))))
    case ShortestPath(playerLoc, find) =>
      sender ! Player.TakePath(shortestPath(playerLoc,find))
    case PrintRooms =>
      var namesAndKeywords = "\nRooms:\t\t\tKeywords:\n"
      for (i <- 0 to names.length - 1) {
        namesAndKeywords += (names(i) + " ")
        for (i <- 0 to 22 - names(i).length) namesAndKeywords += "-"
        namesAndKeywords += keywords(i) + "\n"
      }
      sender ! Player.PrintMessage(namesAndKeywords)
    case m => sender ! Player.PrintMessage("\n*****RoomManager received an unknown message: " + m + "*****")
  }

//  def readRooms(): Map[String, ActorRef] = {
//    val source = scala.io.Source.fromFile("map.txt")
//    val lines = source.getLines()
//    val rooms = Array.fill(lines.next.trim.toInt)(readRoom(lines)).toMap
//    source.close()
//    rooms
//  }
  
  def readRooms(): BSTMap[String, ActorRef] = {
    val source = scala.io.Source.fromFile("map.txt")
    val lines = source.getLines()
    val numRooms = lines.next.trim.toInt
    var treeMap: BSTMap[String,ActorRef] = new BSTMap((a,b)=>a>b)
    for (i <- 1 to numRooms) treeMap += readRoom(lines)
    source.close()
    treeMap
  }

  def readRoom(lines: Iterator[String]): (String, ActorRef) = {
    val keyword = lines.next
    keywords = keyword :: keywords
    val name = lines.next
    names = name :: names
    val desc = lines.next
    val items = List.fill(lines.next.trim.toInt) {
      Item(lines.next, lines.next, lines.next.toInt, lines.next.toInt)
    }
    val exits = lines.next.split(",").map(_.trim)
    connect = connect + (keyword -> exits)
    keyword -> context.actorOf(Props(new Room(name, desc, items, exits)), keyword)
  }

  def shortestPath(playerLocation: String, searchKeyword: String, visited: Set[String] = Set.empty): ArrayBuffer[(Int,String)] = {
    if (searchKeyword == playerLocation) ArrayBuffer((-1,searchKeyword)) else {
      val newVisited = visited + playerLocation
      var ret = (1000000000, ArrayBuffer[(Int,String)]())
      
      for (i <- 0 to 5){
        if (connect(playerLocation)(i) != "None" && !visited(connect(playerLocation)(i))) {
          //ret = ret min 
          val shortPath = shortestPath(connect(playerLocation)(i), searchKeyword, newVisited)
          shortPath += (i -> playerLocation)
          if (shortPath.size < ret._1 && shortPath(0)._2 == searchKeyword) ret = (shortPath.size,shortPath)
        }
      }
      ret._2
    }
  }
}

/*REQUIRED MAP FORMAT:
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
  case class StartRoom(player: ActorRef)
  case class ShortestPath(playerLoc: String, find: String)
  case object PrintRooms
}