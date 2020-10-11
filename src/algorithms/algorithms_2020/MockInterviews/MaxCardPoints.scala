import scala.collection.mutable

case class Index(j : Int,k : Int,n : Int)
object Solution {
  def maxScore(cardPoints: Array[Int], K: Int): Int = {
    var currentSum = 0
    for (j <- 0 to K-1) {
      currentSum = currentSum + cardPoints(j)
    }

    var maxSum = currentSum
    var tail = 0
    for (j <- K to cardPoints.length-1) {
      currentSum = currentSum + cardPoints(j) - cardPoints(tail)
      tail = tail + 1
      maxSum = math.max(currentSum,maxSum)
    }

    maxSum
  }
}