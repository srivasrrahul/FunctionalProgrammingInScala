object Solution {
  def longestPalindrome(s: String): String = {
    if (s.isEmpty) {
      ""
    }else {
      val matrix = Array.ofDim[Boolean](s.length,s.length)
      for (j <- 0 to s.length-1) {
        matrix(j)(j) = true
      }

      var j = 0
      var c = 1
      var k = c

      var longestIndex = (0,0)
      while (j < s.length && k < s.length) {
        if (s(j) != s(k)) {
          matrix(j)(k) = false
        }else {
          if (j+1 == k) {
            matrix(j)(k) = true
          }else {
            matrix(j)(k) = matrix(j + 1)(k - 1)
          }
        }

        if (matrix(j)(k) == true) {
          val diff = k-j+1
          if (diff > (longestIndex._2-longestIndex._1+1)) {
            longestIndex = (j,k)
          }
        }

        j = j + 1
        k = k + 1

        if (k == s.length) {
          c = c + 1
          if (c >= s.length) {
            //Do nothing
          }else {
            j = 0
            k = c
          }
        }
      }

      // for (j <- 0 to s.length-1) {
      //   println(matrix(j).mkString(","))
      // }

      s.substring(longestIndex._1,longestIndex._2+1)
    }
  }
}