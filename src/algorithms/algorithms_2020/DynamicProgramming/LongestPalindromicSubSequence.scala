object Solution {
  def longestPalindromeSubseq(s: String): Int = {
    def lcs(x : String,y : String) : Int = {
      val matrix = Array.ofDim[Int](x.length,y.length)

      for (j <- 0 to x.length-1) {

        if (x(j) == y(0)) {
          matrix(j)(0) = 1
        }else {
          if (j > 0) {
            matrix(j)(0) = matrix(j-1)(0)
          }

        }

      }

      for (k <- 1 to y.length-1) {
        if (y(k) == x(0)) {
          matrix(0)(k) = 1
        }else {
          if (k > 0) {
            matrix(0)(k) = matrix(0)(k-1)
          }
        }

      }

      for (j <- 1 to x.length-1) {
        for (k <- 1 to y.length-1) {
          if (x(j) == y(k)) {
            matrix(j)(k) = 1+ matrix(j-1)(k-1)
          }else {
            matrix(j)(k) = math.max(matrix(j-1)(k),matrix(j)(k-1))
          }
        }
      }

//      for (j <- 0 to matrix.length-1) {
//        println(matrix(j).mkString(","))
//      }

      var j = x.length-1
      var k = x.length-1

      val lcsStr = new StringBuilder
      while (j >= 0 && k >= 0) {
        if (x(j) == y(k)) {
          lcsStr.append(x(j))
          j = j - 1
          k = k -1
        }else {
          if (j > 0 && k > 0) {
            val prev1 = matrix(j)(k-1)
            val prev2 = matrix(j-1)(k)
            if (prev1 > prev2) {
              k = k -1
            }else {
              j = j - 1
            }
          }else {
            if (j > 0) {
              j = j-1
            }else {
              k = k-1
            }
          }
        }
      }

      println(lcsStr.toString())
      matrix(x.length-1)(y.length-1)
    }

    lcs(s,s.reverse)

  }

  def main(args: Array[String]): Unit = {
    println(longestPalindromeSubseq("bbbab"))
  }
}