import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Edge(val source : Int,val dest : Int)
object Solution {
  def shortestPathLength(graph: Array[Array[Int]]): Int = {



    var minPathLen = Int.MaxValue
    def markLowest(newPathLen : Int) : Unit = {
      if (newPathLen < minPathLen) {
        minPathLen = newPathLen
      }
    }

    def findAllPaths(source : Int) : Unit = {


      def explore(current : Int,prevPathLen : Int,edgeVisited : Set[Edge],nodesVisited : Set[Int]) : Unit = {
        //println("Explore " + current + " " + prevPath)
        if (nodesVisited.size == graph.size) {
          //All nodes visited
          markLowest(prevPathLen)
        }else {
          //nodeVisited.add(current)
          for (neigbour <- graph(current)) {
            //println("Neigbour " + neigbour + " " + current)
            val edge = new Edge(current,neigbour)
            //explore(neigbour,current :: prevPath)
            if (edgeVisited.contains(edge) == false) {
              //println("Visit Neigbour " + neigbour + " " + current)
              explore(neigbour,prevPathLen+1,edgeVisited.+(edge),nodesVisited.+(current))
            }
          }
        }
      }

      explore(source,0,Set(),Set())
      //println("For source " + source + " " + pathVisited.toList)
    }

    for (j <- 0 to graph.length-1) {
      findAllPaths(j)
    }

    //findAllPaths(0)


    //println(pathVisited.mkString("\n"))
    if (graph.isEmpty == false && graph.length > 1) {
      minPathLen-1
    }else {
      0
    }





  }

  def main(args: Array[String]): Unit = {
    //val graph = Array(Array(1,2,3),Array(0),Array(0),Array(0))
    val graph = Array(Array(1),Array(0,2,4),Array(1,3,4),Array(2),Array(1,2))
    println(shortestPathLength(graph))
  }
}