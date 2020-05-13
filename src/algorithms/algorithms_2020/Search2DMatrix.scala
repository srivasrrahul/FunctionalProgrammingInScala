import scala.collection.Searching

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.length == 0) {
      false
    }else {
      val maxColSize = matrix(0).length - 1

      def itr(minRowIndex: Int, maxRowIndex: Int): Boolean = {
        //println("minRowIndex " + minRowIndex + " maxRowIndex " + maxRowIndex)
        if (maxRowIndex < minRowIndex) {
          false
        } else {
          if (maxRowIndex == minRowIndex) {
            //println(matrix(minRowIndex).mkString(","))
            matrix(minRowIndex).search(target) match {
              case Searching.Found(_) => {
                true
              }
              case _ => {
                false
              }
            }
          } else {

            val midRowIndex = minRowIndex + (maxRowIndex - minRowIndex) / 2

            val leftRowRange = Range(matrix(minRowIndex)(0), matrix(midRowIndex)(maxColSize) + 1)

            val rightRowRange = Range(matrix(midRowIndex + 1)(0), matrix(maxRowIndex)(maxColSize) + 1)

            //println("Left Row Range " + leftRowRange)
            //println("Right Row Range " + rightRowRange)
            if (leftRowRange.contains(target)) {
              itr(minRowIndex, midRowIndex)
            } else {
              if (rightRowRange.contains(target)) {
                itr(midRowIndex + 1, maxRowIndex)
              } else {
                false
              }
            }


          }
        }
      }

      itr(0,matrix.length-1)
    }


  }

  def main(args: Array[String]): Unit = {
    val array = Array(Array(1,2,3),Array(4,5,6),Array(7,8,9))
    println(searchMatrix(Array(Array(1,2,3)) ,1))
  }
}