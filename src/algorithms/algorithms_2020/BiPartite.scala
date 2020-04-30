import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

case class Edge(val u : Int,val v : Int)

object Solution {

  def isBipartite(graph: Array[Array[Int]]): Boolean = {
    val vertexCount = graph.length

    val set1 = new mutable.HashSet[Int]()
    val set2 = new mutable.HashSet[Int]()

    val visited = new mutable.HashSet[Int]()
    var biPartitlePossible = true

    def explore(current : Int) : Unit = {
      //println("=====++Exploring++===== " + current)
      visited.add(current)

      var canPutInFirst = true
      var canPutInSecond = true

      for (neigbour <- graph(current)) {
        //println("Neigbour " + neigbour + " for " + current)
        if (visited(neigbour) == true) {
          //println("Neigbour " + neigbour + " for " + current + " visited ")
          if (set1.contains(neigbour)) {
            canPutInFirst = false
          }else {
            if (set2.contains(neigbour)) {
              canPutInSecond = false
            }
          }
        }
      }

      (canPutInFirst,canPutInSecond) match {

        case (true,_) => {
          set1.add(current)
        }
        case (_,true) => {
          set2.add(current)
        }
        case (false,false) => {
          //we cant put anywhere
          biPartitlePossible = false
          //println("Makring flse for " + current)
        }
      }

      if (biPartitlePossible == true) {
        for (neigbour <- graph(current)) {
          if (visited.contains(neigbour) == false) {
            explore(neigbour)
          }
        }
      }
    }

    for (j <- 0 to vertexCount-1) {
      if (visited.contains(j) == false) {
        explore(j)
      }

    }

    biPartitlePossible
  }

  def main(args: Array[String]): Unit = {
    val graph = Array(Array(1),Array(0,3),Array(3),Array(1,2))
    println(isBipartite(graph))
  }
}