import scala.collection.mutable

case class Edge(val x : Int,val y : Int)
object Solution {
  def shortestPathLength(graph: Array[Array[Int]]): Int = {
    val edgeDB = new mutable.HashMap[Edge,Int]()
    var id = 0
    for (j <- 0 to graph.length-1) {
      for (k <- graph(j)) {
        val edge = new Edge(j,k)
        edgeDB += ((edge,id))
        id = id + 1
      }
    }

    //println(edgeDB)

    def sp(u : Int,visited : Set[Int],visitedEdgeIds : Set[Int]) : Option[Int] = {
      //println(u + " " + visited + " " + visitedEdgeIds)
      if (visited.size == graph.length) {
        Some(visitedEdgeIds.size)
      }else {
        var globalMin = Int.MaxValue
        for (v <- graph(u)) {
          val edge = new Edge(u,v)
          val edgeId = edgeDB.get(edge).get
          if (visitedEdgeIds.contains(edgeId) == false) {
            val minPath = sp(v,visited.+(u),visitedEdgeIds.+(edgeId))
            if (minPath.isDefined && minPath.get < globalMin) {
              globalMin = minPath.get
            }
          }
        }

        if (globalMin == Int.MaxValue) {
          None
        }else {
          Some(globalMin)
        }
      }
    }

    var globalMin = Int.MaxValue
    for (j <- 0 to graph.length-1) {
      //println("For j " + j)
      val minPath = sp(j,Set(),Set())
      if (minPath.isDefined && minPath.get < globalMin) {
        globalMin = minPath.get
      }
    }

    if (globalMin > 0) {
      globalMin-1
    }else {
      globalMin
    }

  }

  def main(args: Array[String]): Unit = {
    val g = Array(Array(1,2,3),Array(0),Array(0),Array(0))
    println(shortestPathLength(g))
  }
}