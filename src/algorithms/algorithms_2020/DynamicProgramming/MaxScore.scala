import scala.annotation.tailrec
import scala.collection.mutable

case class Index(val left : Int,val right : Int)
object Solution {

  def maxScore(cardPoints: Array[Int], K: Int): Int = {
    if (K == cardPoints.length) {
      cardPoints.sum
    }else {
      var sourceCount = 1 //destCount is 2
      var sourceMatrix = Array.ofDim[Int](cardPoints.length,cardPoints.length)
      var destMatrix = Array.ofDim[Int](cardPoints.length,cardPoints.length)
      for (j <- 0 to cardPoints.length-1) {
        sourceMatrix(j)(j) = cardPoints(j)
      }

      for (j <- 0 to cardPoints.length-1) {
        for (k <- j to cardPoints.length-1) {
          sourceMatrix(j)(k) = math.max(cardPoints(j),cardPoints(k))
        }
      }



      var pickupBase = 2
      var pickupCurrent = pickupBase

      while (pickupCurrent <= K) {
        var j = 0
        var current = pickupCurrent-1
        var k = current

        while (j < cardPoints.length && k < cardPoints.length) {
          destMatrix(j)(k) = math.max(cardPoints(j) + sourceMatrix(j + 1)(k), cardPoints(k) + sourceMatrix(j)(k - 1))

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

        val tempMatrix = sourceMatrix
        sourceMatrix = destMatrix
        destMatrix = tempMatrix
      }


//      for (p <- 1 to K) {
//        println("===============")
//        for (j <- 0 to cardPoints.length-1) {
//          for (k <- 0 to cardPoints.length-1) {
//            print(destMatrix(j)(k) + ",")
//          }
//
//          println()
//        }
//      }

      destMatrix(0)(cardPoints.length-1)

    }


  }
}