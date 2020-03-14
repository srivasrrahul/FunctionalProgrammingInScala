import scala.collection.mutable
import util.control.Breaks._

object Solution {

  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {
    val graph = new mutable.HashMap[Int,List[Int]]()
    var i = 0
    for (lst <- rooms) {
      graph += ((i,lst))
      i += 1
    }

    val distance = new mutable.HashMap[Int,Int]()
    for (j <- 0 to rooms.length-1) {
      distance += ((j,Int.MaxValue))
    }

    distance += ((0,0))
    val visited = new mutable.Queue[Int]()
    visited.addOne(0)

    while (visited.isEmpty == false) {

      val u = visited.removeHead()
      //println("Removed from q " + u)
      graph.get(u) match {
        case Some(neignbours) => {
          neignbours.foreach(neigbour => {
            distance.get(neigbour) match {
              case Some(d) => {
                if (d == Int.MaxValue) {
                  visited.addOne(neigbour)
                  distance += ((neigbour,distance.getOrElseUpdate(u,Int.MaxValue)+1))
                }
              }
              case None => {

              }
            }
          })
        }
        case None => {

        }

      }
    }

    var reachable = true
    breakable {
      distance.foreach(pair => {
        //println(pair._2)
        if (pair._2 == Int.MaxValue) {
          reachable = false
          break()
        }
      })
    }

    reachable
  }

  def main(args: Array[String]): Unit = {
    val l0 = List(1,3)
    val l1 = List(3,0,1)
    val l2 = List(2)
    val l3 = List(0)
    val rooms = List(l0,l1,l2,l3)
    println(canVisitAllRooms(rooms))


  }
}