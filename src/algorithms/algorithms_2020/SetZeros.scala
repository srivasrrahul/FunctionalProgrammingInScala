import scala.collection.mutable

object Solution {
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val rows = new mutable.HashSet[Int]()
    val cols = new mutable.HashSet[Int]()
    for (j <- 0 to  matrix.length-1) {
      for (k <- 0 to matrix(0).length-1) {
        if (matrix(j)(k) == 0) {
          rows.add(j)
          cols.add(k)
        }
      }
    }

    for (j <- 0 to matrix.length-1) {
      for (k <- 0 to matrix(0).length-1) {
        if (rows.contains(j) || cols.contains(k)) {
          matrix(j)(k) = 0
        }
      }
    }
  }
}