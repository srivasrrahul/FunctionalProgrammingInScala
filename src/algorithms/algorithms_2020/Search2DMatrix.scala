import scala.collection.Searching

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.length == 0) {
      false
    }else {
      val maxColSize = matrix(0).length - 1

      def itr(minRowIndex : Int,maxRowIndex : Int) : Boolean = {
        if (minRowIndex > maxRowIndex) {
          false
        } else {
          if (minRowIndex == maxRowIndex) {
            matrix(minRowIndex).search(target) match {
              case Searching.Found(_) => true
              case _ => false
            }
          } else {
            //Search in top
            matrix(minRowIndex).search(target) match {
              case Searching.Found(foundIndex) => true
              case Searching.InsertionPoint(insertionPoint) => {

                matrix(maxRowIndex).search(target) match {
                  case Searching.Found(foundIndex) => {
                    true
                  }
                  case Searching.InsertionPoint(lastInsertionPoint) => {
                    itr(minRowIndex + 1, maxRowIndex - 1)
                  }
                }
              }


            }
          }
        }
      }

      itr(0,matrix.length-1)
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