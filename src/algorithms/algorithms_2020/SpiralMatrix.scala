import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import util.control.Breaks._

case class Index(val x : Int,val y : Int)
object Solution {
  def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
    val rowSize = matrix.length
    if (rowSize == 0) {
      List[Int]()
    }else {
      val colSize = matrix(0).length

      if (colSize == 0) {
        List[Int]()

      } else {

        val visited = new mutable.HashSet[Index]()
        var currentRowBegin = 0
        var currentRowEnd = rowSize - 1

        var currentColBegin = 0
        var currentColEnd = colSize - 1


        val path = new ListBuffer[Int]

        breakable {
          while (true) {
            //println("In one iteration " + currentRowBegin + "," + currentRowEnd + "   " + currentColBegin + "," + currentColEnd )
            var shouldBreak = false
            breakable {
              for (j <- currentColBegin to currentColEnd) {
                val currentNodeIndex = new Index(currentColBegin, j)
                if (visited.contains(currentNodeIndex) == true) {
                  shouldBreak = true
                  break
                }

                visited.add(currentNodeIndex)

                //println(currentNodeIndex)

                path.addOne(matrix(currentColBegin)(j))
              }
            }

            if (shouldBreak) {
              break
            }

            breakable {
              for (j <- currentRowBegin + 1 to currentRowEnd) {
                val currentNodeIndex = new Index(j, currentColEnd)


                if (visited.contains(currentNodeIndex) == true) {
                  shouldBreak = true
                  break
                }
                visited.add(currentNodeIndex)
                //println(currentNodeIndex)

                path.addOne(matrix(j)(currentColEnd))
              }
            }

            if (shouldBreak) {
              break
            }

            breakable {
              for (j <- currentColEnd - 1 to currentColBegin by -1) {
                val currentNodeIndex = new Index(currentRowEnd, j)


                if (visited.contains(currentNodeIndex) == true) {
                  shouldBreak = true
                  break
                }

                visited.add(currentNodeIndex)

                //println(currentNodeIndex)

                path.addOne(matrix(currentRowEnd)(j))
              }
            }

            if (shouldBreak) {
              break
            }

            breakable {
              for (j <- currentRowEnd - 1 to currentRowBegin + 1 by -1) {
                val currentNodeIndex = new Index(j, currentColBegin)


                if (visited.contains(currentNodeIndex) == true) {
                  shouldBreak = true
                  break
                }

                visited.add(currentNodeIndex)

                //println(currentNodeIndex)
                path.addOne(matrix(j)(currentColBegin))
              }

              if (shouldBreak) {
                break
              }
            }

            currentColBegin = currentColBegin + 1
            currentColEnd = currentColEnd - 1

            currentRowBegin = currentRowBegin + 1
            currentRowEnd = currentRowEnd - 1

            if (visited.size >= rowSize * colSize) {
              break
            }
          }
        }

        path.toList
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //println(spiralOrder(Array(Array(1,2,3,4),Array(5,6,7,8),Array(9,10,11,12))))
    //println(spiralOrder(Array(Array[Int](1),Array[Int](2),Array[Int](3))))
    println(spiralOrder(Array[Array[Int]]()))
  }
}