object Solution {
  def isOneEditDistance(s: String, t: String): Boolean = {
    //Plain edit distance
    val matrix = Array.ofDim[Int](s.length+1,t.length+1)

    for (j <- 0 to t.length) {
      matrix(0)(j) = j
    }

    for (j <- 0 to s.length) {
      matrix(j)(0) = j
    }

    for (j <- 1 to s.length) {
      for (k <- 1 to t.length) {
        val option1 = matrix(j-1)(k) + 1
        val option2 = matrix(j)(k-1) + 1

        val diff = if (s(j-1) == t(k-1)) 0 else 1
        val option3 = matrix(j-1)(k-1) + diff
        matrix(j)(k) = Array(option1,option2,option3).min
      }
    }

//    for (j <- 0 to matrix.length-1) {
//      println(matrix(j).mkString(","))
//    }

    matrix(s.length)(t.length) == 1
  }



  def main(args: Array[String]): Unit = {
    println(isOneEditDistance("abc","ab"))
  }
}