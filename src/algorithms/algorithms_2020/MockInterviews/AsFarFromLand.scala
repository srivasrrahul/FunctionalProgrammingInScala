import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Solution {
  def maxDistance(grid: Array[Array[Int]]): Int = {
    //run bfs from each water land and stor in TreeSet
    val rows = grid.length
    val cols = grid(0).length

    def getIndex(vertexId : Int) : (Int,Int) = {
      (vertexId/cols,vertexId % cols)
    }


    def getVertex(j : Int,k : Int) : Int = {
      j*cols+k
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


    def bfs(vertex : Int) : Int = { //path is water
      //min path from nearest land
      val pathLens = new mutable.HashMap[Int,Int]()
      val q = new mutable.Queue[Int]()

      q.clear()
      q.append(vertex)

      pathLens.clear()

      //val visited = new mutable.HashSet[Int]()
      //val pathLens = new mutable.HashMap[Int,Int]()
      pathLens += ((vertex,0))

      var minPath = Int.MaxValue
      while (q.isEmpty == false && minPath == Int.MaxValue) {
        val top = q.dequeue()
        //visited.add(top)

        val existingPathLen = pathLens.get(top).get //has to be there
        for (v <- getNeigbour(top)) {
          val (j1,k1) = getIndex(v)
          if (pathLens.contains(v) == false) { //its a land found it
            pathLens += ((v,existingPathLen+1))
            q.append(v)
            if (grid(j1)(k1) == 1) {
              //its a land
              minPath = existingPathLen+1
            }
          }
        }
      }


      //      for ((v,pathLen) <- pathLens) {
      //        val (j,k) = getIndex(v)
      //        if (grid(j)(k) == 1 && pathLen < minPath) { //land
      //          minPath = pathLen
      //        }
      //      }
      //
      //      pathLens.clear()

      minPath
    }

    var maxDistance = -1
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {
        if (grid(j)(k) == 0) {
          //println(j + " " + k)
          val minPath = bfs(getVertex(j,k))
          if (minPath != Int.MaxValue && minPath > maxDistance) {
            maxDistance = minPath
          }
        }
      }
    }

    maxDistance



  }
}