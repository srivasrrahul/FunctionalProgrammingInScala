import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val j : Int,val k : Int)
object Solution {
  def uniquePathsIII(grid: Array[Array[Int]]): Int = {
    if (grid.isEmpty) {
      0
    }else {
      val rows = grid.length
      val cols = grid(0).length
      var begin: Index = null
      var end: Index = null
      val validSets = new mutable.HashSet[Index]()
      for (j <- 0 to rows - 1) {
        for (k <- 0 to cols - 1) {
          if (grid(j)(k) == 1) {
            begin = new Index(j, k)
          }

          if (grid(j)(k) == 2) {
            end = new Index(j, k)
          }

          if (grid(j)(k) != -1 && grid(j)(k) != 2) {
            validSets.add(new Index(j, k))
          }
        }
      }

      def next(source: Index): List[Index] = {
        val nextLst = new ListBuffer[Index]

        val x = source.j
        val y = source.k

        if (x - 1 >= 0 && grid(x - 1)(y) != -1) {
          nextLst.append(new Index(x - 1, y))
        }

        if (x + 1 < rows && grid(x + 1)(y) != -1) {
          nextLst.append(new Index(x + 1, y))
        }

        if (y - 1 >= 0 && grid(x)(y - 1) != -1) {
          nextLst.append(new Index(x, y - 1))
        }

        if (y + 1 < cols && grid(x)(y + 1) != -1) {
          nextLst.append(new Index(x, y + 1))
        }

        nextLst.toList
      }

      var count = 0

      def explore(current: Index, visited: Set[Index]): Unit = {
        if (current == end) {
          if (visited.size == validSets.size) {
            count = count + 1 //reached here
          }

        } else {
          val nextIndexes = next(current)
          for (nextIndex <- nextIndexes) {
            if (visited.contains(current) == false) {
              explore(nextIndex, visited.+(current))
            }
          }
        }
      }

      explore(begin, Set())
      count
    }
  }
}