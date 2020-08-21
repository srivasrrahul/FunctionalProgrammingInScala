import scala.annotation.tailrec
import scala.collection.mutable

case class Index(val left : Int,val right : Int)
object Solution {

  def maxScore(cardPoints: Array[Int], K: Int): Int = {
    if (K == cardPoints.length) {
      cardPoints.sum
    }else {
      val matrix = Array.ofDim[Int](cardPoints.length,cardPoints.length,K+1)
      for (j <- 0 to cardPoints.length-1) {
        matrix(j)(j)(1) = cardPoints(j)
      }

      for (j <- 0 to cardPoints.length-1) {
        for (k <- j to cardPoints.length-1) {
          cardPoints(j)(k)(1) = math.max(cardPoints(j),cardPoints(k))
        }
      }



      var pickupBase = 2
      var pickupCurrent = pickupBase

      while (pickupCurrent <= K) {
        var j = 0
        var current = pickupCurrent-1
        var k = current

        while (j < cardPoints.length && k < cardPoints.length) {
          matrix(j)(k)(pickupCurrent) = math.max(cardPoints(j) + matrix(j + 1)(k)(pickupCurrent - 1), cardPoints(k) + matrix(j)(k - 1)(pickupCurrent - 1))

          j = j + 1
          k = k + 1

          if (k >= cardPoints.length) {
            current = current + 1
            j = 0
            k = current
          }
        }

        pickupBase = pickupBase + 1
        pickupCurrent = pickupBase
      }


      for (p <- 1 to K) {
        println("===============")
        for (j <- 0 to cardPoints.length-1) {
          for (k <- 0 to cardPoints.length-1) {
            print(matrix(j)(k)(p) + ",")
          }

          println()
        }
      }

      matrix(0)(cardPoints.length-1)(K)

    }


  }
}