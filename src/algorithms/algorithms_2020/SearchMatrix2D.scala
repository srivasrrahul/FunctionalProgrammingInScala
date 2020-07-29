import scala.collection.Searching

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.isEmpty) {
      false
    }else {
      val cols = matrix(0).length - 1

      def search(rowBegin: Int, rowEnd: Int): Boolean = {
        if (rowBegin == rowEnd) {
          matrix(rowBegin).search(target) match {
            case Searching.Found(_) => true
            case _ => false
          }
        } else {
          val mid = rowBegin + (rowEnd - rowBegin) / 2
          if (target <= matrix(mid)(cols)) {
            search(rowBegin, mid)
          } else {
            search(mid + 1, rowEnd)
          }
        }
      }

      search(0, matrix.length - 1)
    }
  }
}