object Solution {
  def minDistance(word1: String, word2: String): Int = {
    val rowSize = word1.length+1
    val colSize = word2.length+1
    val matrix = Array.ofDim[Int](rowSize,colSize)

    for (j <- 0 to colSize-1) {
      matrix(0)(j) = j
    }

    for (k <- 0 to rowSize-1) {
      matrix(k)(0) = k
    }

    for (j <- 1 to rowSize-1) {
      for (k <- 1 to colSize-1) {
        val firstIndexChar = word1(j-1)
        val secondIndexChar = word2(k-1)
        val cost1 = 1 + matrix(j-1)(k)
        val cost2 = 1 + matrix(j)(k-1)
        var diffCost = 1
        if (firstIndexChar == secondIndexChar) {
          diffCost = 0
        }

        val cost3 = diffCost + matrix(j-1)(k-1)
        val minCost = Array(cost1,cost2,cost3).min
        matrix(j)(k) = minCost

      }
    }

    matrix(rowSize-1)(colSize-1)
  }

  def main(args: Array[String]): Unit = {

    println(minDistance("horse","ros"))
  }
}