import scala.collection.Searching

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.length == 0) {
      false
    }else {
      val maxColSize = matrix(0).length - 1
      var rowIndex = matrix.length-1
      var colIndex = maxColSize
      var found = false
      while (rowIndex >= 0 && colIndex <= maxColSize && found == false) {
        if (matrix(rowIndex)(0) > target) {
          rowIndex = rowIndex-1
        }else {
          if (matrix(rowIndex)(0) < target) {
            colIndex = colIndex+1
          }else {
            found = true
          }
        }
      }

      found

    }


  }

  def main(args: Array[String]): Unit = {
    val array = Array(Array(1,4,7,11,15),
      Array(2,5,8,12,19),
      Array(3,6,9,16,22),
      Array(10,13,14,17,24),
      Array(18,21,23,26,30))
    println(searchMatrix(array ,-1))
  }
}