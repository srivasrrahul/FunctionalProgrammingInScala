object Solution {

  def longestCommonSubsequence(text1: String, text2: String): Int = {
    val matrix = Array.ofDim[Int](text1.length,text2.length)
    if (text1.charAt(0) == text2.charAt(0)) {
      matrix(0)(0) = 1
    }else {
      matrix(0)(0) = 0
    }

    for (j <- 1 to text2.length-1) {

      if (text1.charAt(0) == text2.charAt(j)) {
        matrix(0)(j) = 1
      }else {
        matrix(0)(j) = matrix(0)(j-1)
        //0
      }
    }

    for (j <- 1 to text1.length-1) {

      if (text1.charAt(j) == text2.charAt(0)) {
        matrix(j)(0) = 1
      }else {
        matrix(j)(0) = matrix(j-1)(0)
        //0
      }
    }


    var max_val = 0
    for (j <- 1 to text1.length-1) {
      for (k <- 1 to text2.length-1) {
        if (text1.charAt(j) == text2.charAt(k)) {
          matrix(j)(k) = 1 + matrix(j-1)(k-1)
        }else {
          matrix(j)(k) = math.max(matrix(j)(k-1),matrix(j-1)(k))
        }

        if (matrix(j)(k) > max_val) {
          max_val = matrix(j)(k)
        }
      }
    }





    //    print("  ")
    //    println(text2.mkString(","))
    //    for (j <- 0 to text1.length-1) {
    //      for (k <- 0 to text2.length-1) {
    //        if (k == 0) {
    //          print(text1.charAt(j) + ",")
    //        }
    //
    //        print(matrix(j)(k) + ",")
    //
    //      }
    //      println()
    //    }

    max_val
    //0



  }
  def longestPalindromeSubseq(s: String): Int = {
    if (s.length == 1) {
      1
    }else {
      longestCommonSubsequence(s,s.reverse)
    }

  }

  def main(args: Array[String]): Unit = {
    println(longestPalindromeSubseq("a"))
  }
}