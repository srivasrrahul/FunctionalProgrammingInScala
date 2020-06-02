import scala.collection.Searching

object Solution {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    if (matrix.length == 0) {
      false
    }else {
      val maxColSize = matrix(0).length - 1

      def itr(minRowIndex : Int,maxRowIndex : Int,minColIndx : Int,maxColIndex : Int) : Boolean = {
        println(minRowIndex + " " + maxRowIndex + " " + minColIndx + " " + maxColIndex)
        if (minRowIndex > maxRowIndex || minColIndx > maxColIndex) {
          false
        }else {
          if (minRowIndex == maxRowIndex && minColIndx == maxColIndex) {
            matrix(minRowIndex)(minColIndx) == target
          } else {
            val topRowRange = Range(matrix(minRowIndex)(minColIndx), matrix(minRowIndex)(maxColIndex) + 1)
            val bottomRowRange = Range(matrix(maxRowIndex)(minColIndx), matrix(maxRowIndex)(maxColIndex) + 1)

            val leftColRange = Range(matrix(minRowIndex)(minColIndx), matrix(maxRowIndex)(minColIndx) + 1)
            val rightColRange = Range(matrix(minRowIndex)(maxColIndex), matrix(maxRowIndex)(maxColIndex) + 1)

            //Can eliminate topRow
            var newMinRowIndex = minRowIndex
            if (topRowRange.contains(target) == false) {
              newMinRowIndex = minRowIndex + 1
            }

            var newMaxRowIndex = maxRowIndex
            if (bottomRowRange.contains(target) == false) {
              newMaxRowIndex = maxRowIndex - 1
            }

            var newMinColIndex = minColIndx
            if (leftColRange.contains(target) == false) {
              newMinColIndex = newMinColIndex + 1
            }

            var newMaxColIndex = maxColIndex
            if (rightColRange.contains(target) == false) {
              newMaxColIndex = newMaxColIndex - 1
            }

            itr(newMinRowIndex, newMaxRowIndex, newMinColIndex, newMaxColIndex)
          }
        }
      }

      itr(0,matrix.length-1,0,matrix(0).length-1)
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