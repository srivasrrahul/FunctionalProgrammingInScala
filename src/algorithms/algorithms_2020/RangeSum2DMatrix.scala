import scala.collection.mutable

class NumMatrix(_matrix: Array[Array[Int]]) {
  //sum of all rows and cols separately
  val rowSum = new mutable.HashMap[Int,Array[Int]]()
  for (j <- 0 to _matrix.length-1) {
    val arr = new Array[Int](_matrix(0).length)
    arr(0) = _matrix(j)(0)
    for (k <- 1 to _matrix(0).length-1) {
      arr(k) = arr(k-1) + _matrix(j)(k)
    }

    rowSum += ((j,arr))
  }

  // for ((k,v) <- rowSum) {
  //     println("for k " + k + " " + v.mkString(","))
  // }

  def sumRegion(row1: Int, col1: Int, row2: Int, col2: Int): Int = {
    var sum = 0
    for (j <- row1 to row2) {
      val currentSumArr = rowSum.get(j).get
      var validSum = currentSumArr(col2)
      if (col1 > 0) {
        validSum = validSum - currentSumArr(col1-1)
      }

      sum = sum + validSum
    }

    sum
  }

}

/**
 * Your NumMatrix object will be instantiated and called as such:
 * var obj = new NumMatrix(matrix)
 * var param_1 = obj.sumRegion(row1,col1,row2,col2)
 */