object Solution {
  def minPathSum(grid: Array[Array[Int]]): Int = {
    val rows = grid.length
    val cols = grid(0).length
    val matrix = Array.ofDim[Int](rows,cols)

    matrix(rows-1)(cols-1) = grid(rows-1)(cols-1)
    for (j <- cols-2 to 0 by -1) {
      matrix(rows-1)(j) = grid(rows-1)(j) + matrix(rows-1)(j+1)
    }

    for (j <- rows-2 to 0 by -1) {
      matrix(j)(cols-1) = grid(j)(cols-1) + matrix(j+1)(cols-1)
    }


    for (j <- rows-2 to 0 by -1) {
      for (k <- cols-2 to 0 by -1) {
        matrix(j)(k) = grid(j)(k) + scala.math.min(matrix(j+1)(k),matrix(j)(k+1))
      }
    }

    matrix(0)(0)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(Array(1,3,1),Array(1,5,1),Array(4,2,1))
    println(minPathSum(arr))
  }
}