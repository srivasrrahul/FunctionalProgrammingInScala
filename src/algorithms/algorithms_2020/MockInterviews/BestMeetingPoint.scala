import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def minTotalDistance(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = grid(0).length


    def manhattan(j : Int,k : Int,j1 : Int,k1 : Int) : Int = {
      math.abs(j-j1) + math.abs(k-k1)
    }

    var minDistance = Int.MaxValue
    for (j <- 0 to rows-1) {
      for (k <- 0 to cols-1) {

        var distance = 0
        for (j1 <- 0 to rows-1) {
          for (k1 <- 0 to cols-1) {
            if (grid(j1)(k1) == 1) {
              distance = distance + manhattan(j,k,j1,k1)
            }
          }
        }

        if (distance < minDistance) {
          minDistance = distance
        }
      }
    }

    minDistance
  }
}