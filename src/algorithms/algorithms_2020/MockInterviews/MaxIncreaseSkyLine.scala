import scala.collection.mutable

object Solution {
  def maxIncreaseKeepingSkyline(grid: Array[Array[Int]]): Int = {
    val colMap = new mutable.HashMap[Int,Int]()
    val rowMap = new mutable.HashMap[Int,Int]()

    for (j <- 0 to grid.length-1) {
      var maxRow = grid(j)(0)
      for (k <- 0 to grid(0).length-1) {
        if (grid(j)(k) > maxRow) {
          maxRow = grid(j)(k)
        }

        val maxCol = colMap.getOrElseUpdate(k,grid(j)(k))
        if (grid(j)(k) > maxCol) {
          colMap += ((k,grid(j)(k)))
        }
      }

      rowMap += ((j,maxRow))
    }

    var sum = 0
    for (j <- 0 to grid.length-1) {
      for (k <- 0 to grid(0).length-1) {
        if (rowMap.get(j).get > grid(j)(k) && colMap.get(k).get > grid(j)(k)) {
          val diff =  math.min(rowMap.get(j).get,colMap.get(k).get) - grid(j)(k)
          sum = sum + diff
        }
      }
    }

    sum
  }
}