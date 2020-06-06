import scala.collection.mutable
case class Edge(val source : Int,val cost : Int)

//need to have inverted index i.e. given a destination give me all edges
class Graph {
  val nodeEdges = new mutable.HashMap[Int,List[Edge]]()

  def addEdge(array: Array[Int]) : Unit = {
    val defaultLst = nodeEdges.getOrElseUpdate(array(1),List[Edge]())
    val newEdge = new Edge(array(0),array(2))
    nodeEdges += ((array(1), newEdge :: defaultLst ))
  }

  def getEdges(destNode : Int) : List[Edge] = {
    nodeEdges.get(destNode) match {
      case Some(lst) => lst
      case None => List()
    }
  }
}

object Solution {
  def findCheapestPrice(n: Int, flights: Array[Array[Int]], src: Int, dst: Int, K: Int): Int = {
    val graph = new Graph
    for (flight <- flights) {
      graph.addEdge(flight)
    }

    //println(graph.nodeEdges)
    val matrix = Array.ofDim[Int](n, K + 1)
    for (j <- 0 to n - 1) {
      for (k <- 0 to K) {
        matrix(j)(k) = Int.MaxValue
      }
    }

    matrix(src)(0) = 0

    var globalUpdated = new mutable.HashSet[Int]()
    for (k <- 0 to n-1) {
      val sourceEdges = graph.getEdges(k)
      for (sourceEdge <- sourceEdges) {
        if (sourceEdge.source == src) {
          matrix(k)(0) = sourceEdge.cost
          globalUpdated.add(k)
        }
      }
    }
    //println("Global Updated " + globalUpdated)
    for (j <- 1 to K) {
      val currentUpdated = new mutable.HashSet[Int]()
      for (k <- 0 to n-1) {
        val sourceEdges = graph.getEdges(k)
        var minPathLength = Int.MaxValue
        for (sourceEdge <- sourceEdges) {
          if ( matrix(sourceEdge.source)(j-1) != Int.MaxValue) {
            val updatedPathLength = sourceEdge.cost + matrix(sourceEdge.source)(j - 1)
            //println(" updatedPathLength " + updatedPathLength + " " + sourceEdge)
            if (updatedPathLength < minPathLength) {
              minPathLength = updatedPathLength
            }
          }
        }

        if (minPathLength < matrix(k)(j)) {
          currentUpdated.add(k)
          matrix(k)(j) = minPathLength
        }
      }

      globalUpdated = currentUpdated
    }

    // for (j <- 0 to matrix.length-1) {
    //   println(matrix(j).mkString(","))
    // }

    val minValue = matrix(dst).min
    if (minValue == Int.MaxValue) {
      -1
    }else {
      minValue
    }
  }


  def main(args: Array[String]): Unit = {
    val flights = Array(Array(0,1,100),Array(1,2,100),Array(0,2,500))
    println(findCheapestPrice(3,flights,0,2,2))

  }
}