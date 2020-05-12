object Solution {

  def rotate(matrix: Array[Array[Int]]): Unit = {
    val n = matrix.length
    if (n <= 1) {
      //
    }else {
      def shiftMatrix(j : Int) : Unit = {
        //shift first
        val leftUpperCorner = (j,j)
        val rightUpperCorner = (j,n-1-j)
        val rightLowerCorner = (n-1-j,n-1-j)
        val leftLowerCorner = (n-1-j,j)

//        println(leftUpperCorner)
//        println(rightUpperCorner)
//        println(rightLowerCorner)


        for (x <- leftUpperCorner._2 to rightUpperCorner._2-1) {

          val diff = x-leftUpperCorner._2
//          println("diff is " + diff)
          val tempLeftUpperCorner = matrix(leftUpperCorner._1)(leftUpperCorner._2+diff)
          var tempRightUpperCorner = matrix(rightUpperCorner._1+diff)(rightUpperCorner._2)
          val tempRightLowerCorner = matrix(rightLowerCorner._1)(rightLowerCorner._2-diff)
          val tempLeftLowerCorner = matrix(leftLowerCorner._1-diff)(leftLowerCorner._2)

//          println("tempLeftUpperCorner " + tempLeftUpperCorner)
//          println("tempRightUpperCorner " + tempRightUpperCorner)

          matrix(rightUpperCorner._1+diff)(rightUpperCorner._2) = tempLeftUpperCorner
          matrix(rightLowerCorner._1)(rightLowerCorner._2-diff) = tempRightUpperCorner
          matrix(leftLowerCorner._1-diff)(leftLowerCorner._2) = tempRightLowerCorner
          matrix(leftUpperCorner._1)(leftUpperCorner._2+diff) = tempLeftLowerCorner

//          for (arr <- matrix) {
//            println(arr.mkString(","))
//          }


        }
      }

      for (k <- 0 to n-1) {
        //println("For k " + k)
        shiftMatrix(k)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array(Array(1,2,3),Array(4,5,6),Array(7,8,9))
    //val matrix = Array(Array(1,2,3,4),Array(5,6,7,8),Array(9,10,11,12),Array(13,14,15,16))
    rotate(matrix)

    for (arr <- matrix) {
      println(arr.mkString(","))
    }

  }
}