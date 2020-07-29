import scala.collection.Searching

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    def search(rowNum : Int) : Boolean = {
      if (rowNum >= matrix.length) {
        false
      }else {
        matrix(rowNum).search(target) match {
          case Searching.Found(foundIndex) => {
            true
          }
          case _ => {
            search(rowNum+1)
          }
        }
      }
    }

    search(0)
  }
}