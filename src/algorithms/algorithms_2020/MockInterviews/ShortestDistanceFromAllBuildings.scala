import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def shortestDistance(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = grid(0).length

    def getVertex(j : Int,k : Int) : Int = {
      j*cols+k
    }

    def getIndex(vertex : Int) : (Int,Int) = {
      (vertex/cols,vertex%cols)
    }

    def getNeigbour(vertex : Int) : List[Int] = {
      val (j,k) = getIndex(vertex)
      val lst = new ListBuffer[Int]
      if (j-1 >= 0) {
        lst.append(getVertex(j-1,k))
      }

      if (j+1 < rows) {
        lst.append(getVertex(j+1,k))
      }

      if (k-1 >= 0) {
        lst.append(getVertex(j,k-1))
      }

      if (k+1 < cols) {
        lst.append(getVertex(j,k+1))
      }

      lst.toList
    }

    val allBuildings = new mutable.HashSet[Int]()
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {
        if (grid(j)(k) == 1) {

          allBuildings.add(getVertex(j,k))
        }
      }
    }
    //println(allBuildings)
    def bfs(s : Int) : Int = {
      //source is empty
      val q = new mutable.Queue[Int]()
      val pathLen = new mutable.HashMap[Int,Int]()

      q.append(s)
      pathLen += ((s,0))
      var minDist = Int.MaxValue

      while (q.isEmpty == false && minDist == Int.MaxValue) {
        val top = q.dequeue()

        val existingPathlen = pathLen.get(top).get
        for (v <- getNeigbour(top)) {
          if (pathLen.contains(v) == false) {
            val (j1,k1) = getIndex(v)
            pathLen += ((v,existingPathlen+1))
            if (grid(j1)(k1) == 0)  {
              q.append(v)
            }
          }
        }

      }

      //Check if this is reachable to all buildings

      val pathVertices = pathLen.keySet
      if (allBuildings.subsetOf(pathVertices)) {
        var distance = 0
        for (building <- allBuildings) {
          distance = distance + pathLen.get(building).get
        }

        distance
      }else {
        -1
      }


    }

    var shortestDistance = Int.MaxValue
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {
        if (grid(j)(k) == 0) {
          //open land
          val distance = bfs(getVertex(j,k))
          if (distance != -1) {
            if (distance < shortestDistance) {
              shortestDistance = distance
            }
          }
        }
      }
    }

    if (shortestDistance == Int.MaxValue) {
      -1
    }else {
      shortestDistance
    }
  }
}