import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def minTotalDistance(grid: Array[Array[Int]]): Int = {
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

    def bfs(s : Int) : Int = {
      //source is an empty point
      val q = new mutable.Queue[Int]()
      val pathLen = new mutable.HashMap[Int,Int]()

      q.append(s)
      pathLen += ((s,0))

      while (q.isEmpty == false) {
        val top = q.dequeue()
        val existingPath = pathLen.get(top).get

        for (v <- getNeigbour(top)) {
          if (pathLen.contains(v) == false) {
            pathLen += ((v,existingPath+1))
            q.append(v)
          }
        }
      }

      var distance = 0
      for ((v,len) <- pathLen) {
        val (j,k) = getIndex(v)
        if (grid(j)(k) == 1) {
          distance = distance + len
        }
      }

      distance
    }

    var minDistance = Int.MaxValue
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {
        val distance = bfs(getVertex(j,k))
        if (distance < minDistance) {
          minDistance = distance
        }
      }
    }

    minDistance
  }
}