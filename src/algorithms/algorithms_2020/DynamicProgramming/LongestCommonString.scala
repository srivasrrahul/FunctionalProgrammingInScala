object Solution {

  def lcs(x: Array[Int], y: Array[Int]): Int = {
    //println(x + " " + y)
    val matrix = Array.ofDim[(Int, Boolean)](x.length, y.length)
    for (j <- 0 to x.length - 1) {
      if (x(j) == y(0)) {
        matrix(j)(0) = (1, true)
      } else {
        if (j > 0) {
          matrix(j)(0) = (matrix(j - 1)(0)._1, false)
        } else {
          matrix(j)(0) = (0, false)
        }
      }
    }

    for (k <- 0 to y.length - 1) {
      if (y(k) == x(0)) {
        matrix(0)(k) = (1, true)
      } else {
        if (k > 0) {
          matrix(0)(k) = (matrix(0)(k - 1)._1, false)
        } else {
          matrix(0)(k) = (0, false)
        }
      }
    }

//    for (j <- 0 to x.length-1) {
//      println(matrix(j).mkString(","))
//    }

    for (j <- 1 to x.length - 1) {
      for (k <- 1 to y.length - 1) {
        if (x(j) == y(k)) {
          if (matrix(j - 1)(k - 1)._2 == true) {
            matrix(j)(k) = (matrix(j - 1)(k - 1)._1 + 1, true)
          } else {
            matrix(j)(k) = (1, true)
          }
        } else {
          val maxValue = math.max(matrix(j - 1)(k)._1, matrix(j)(k - 1)._1)
          matrix(j)(k) = (maxValue, false)
        }
      }
    }

//          for (j <- 0 to x.length-1) {
//            println(matrix(j).mkString(","))
//          }

    var maxLen = 0
    val lcsStr = new StringBuilder
    var xIndex = -1
    for (j <- x.length - 1 to 0 by -1) {
      for (k <- y.length - 1 to 0 by -1) {
        if (matrix(j)(k)._1 >= maxLen && matrix(j)(k)._2 == true) {
          maxLen = matrix(j)(k)._1
          xIndex = j
        }
      }
    }

    maxLen

    //matrix(x.length-1)(y.length-1)._1
  }


  def findLength(A: Array[Int], B: Array[Int]): Int = {

    lcs(A,B)

  }

  def main(args: Array[String]): Unit = {
    println(findLength(Array(1,2,3,2,1),Array(3,2,1,4,7)))
  }

}



