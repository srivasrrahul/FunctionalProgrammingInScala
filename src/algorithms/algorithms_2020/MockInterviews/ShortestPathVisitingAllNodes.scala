import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val s : Set[Int],val u : Int)
object Solution {
  def shortestPathLength(graph: Array[Array[Int]]): Int = {
    val allPairShortestPaths = Array.ofDim[Int](graph.length,graph.length)
    for (j <- 0 to graph.length-1) {
      for (k <- 0 to graph.length-1) {
        allPairShortestPaths(j)(k) = Int.MaxValue
      }
    }

    for (j <- 0 to graph.length-1) {
      allPairShortestPaths(j)(j) = 0
    }

    for (u <- 0 to graph.length-1) {
      for (v <- graph(u)) {
        allPairShortestPaths(u)(v) = 1
      }
    }

    for (k <- 0 to graph.length-1) {
      for (i <- 0 to graph.length-1) {
        for (j <- 0 to graph.length-1) {
          val p1 = allPairShortestPaths(i)(k)
          val p2 = allPairShortestPaths(k)(j)
          if (p1 != Int.MaxValue && p2 != Int.MaxValue) {
            val p = p1 + p2
            if (p < allPairShortestPaths(i)(j)) {
              allPairShortestPaths(i)(j) = p
            }
          }
        }
      }
    }



    val cache = new mutable.HashMap[Index,Int]()
    def sp(notVisted : Set[Int],current : Int) : Int = {
      if (notVisted.size == 0) {
        0
      }else {
        if (notVisted.size == 1) {
          allPairShortestPaths(notVisted.head)(current)
        }else {
          val index = new Index(notVisted,current)
          if (cache.contains(index)) {
            cache.get(index).get
          }else {
            var minCost = Int.MaxValue
            for (u <- notVisted) {
              val cost = allPairShortestPaths(u)(current) + sp(notVisted.-(u), u)
              if (cost < minCost) {
                minCost = cost
              }
            }

            cache += ((index,minCost))
            minCost
          }
        }
      }
    }

    val S = Range(0,graph.length).toSet
    var minCost = Int.MaxValue
    for (j <- graph.length-1 to 0 by -1) {
      val cost = sp(S.-(j),j)
      if (cost < minCost) {
        minCost = cost
      }
    }

    minCost

  }

  def main(args: Array[String]): Unit = {
    val g = Array(Array(1,2,3),Array(0),Array(0),Array(0))
    println(shortestPathLength(g))
  }
}