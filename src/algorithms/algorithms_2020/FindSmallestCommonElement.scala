import scala.collection.Searching

object Solution {
  def smallestCommonElement(matrix: Array[Array[Int]]): Int = {
    var retValue = matrix(0)(0)
    var found = false
    for (k <- 0 to matrix(0).length-1 if found == false) {
      retValue = matrix(0)(k)
      found = true
      for (j <- 1 to matrix.length-1 if found == true) {
        matrix(j).search(retValue) match {
          case Searching.Found(_) => {

          }
          case _  => {
            found = false
          }
        }
      }
    }

    if (found == false) {
      -1
    }else {
      retValue
    }
  }
}