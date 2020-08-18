import scala.collection.mutable

case class Index(val begin : Int,val end : Int,val pendingMailBox : Int)
object Solution {
  def minDistance(houses: Array[Int], mbCount: Int): Int = {
    houses.sortInPlace()
    val matrix = Array.ofDim[Int](houses.length,houses.length,mbCount+1)
    for (j <- 0 to houses.length-1) {
      for (k <- j to houses.length-1) {
        for (p <- 1 to mbCount) {
          val count = k-j+1
          if (p >= count) {
            matrix(j)(k)(p) = 0
          }else {
            if (count == 1) {
              matrix(j)(k)(p) = 0
            }else {
              if (p == 1) {
                var midValue = 0
                if (count % 2 == 0) {
                  val mid = j + (k - j) / 2
                  val mid1 = mid + 1
                  midValue = (houses(mid) + houses(mid + 1)) / 2
                } else {
                  val mid = j + (k - j) / 2
                  midValue = houses(mid)
                }

                var distance = 0
                for (x <- j to k) {
                  distance = distance + math.abs(houses(x) - midValue)
                }

                matrix(j)(k)(p) = distance
              }

            }
          }
        }
      }
    }

    var j = 0
    var current = 0
    var k = current

    while (j < houses.length && k < houses.length) {
      for (p <- 1 to mbCount if (k-j+1 > 1 && p > 1)) {
        var minSum = Int.MaxValue
        for (x <- j to k) {
          for (pending <- 1 to p-1) {
            val leftSum = matrix(j)(x)(pending)
            var rightSum = 0
            if (x+1 <= k) {
              rightSum = matrix(x+1)(k)(p-pending)
            }

            if ((leftSum+rightSum) < minSum) {
              minSum = leftSum + rightSum
            }
          }
        }

        matrix(j)(k)(p) = minSum
      }

      j = j + 1
      k = k + 1

      if (k >= houses.length) {
        current = current+1
        j = 0
        k = current
      }
    }


//    for (j <- 0 to houses.length-1) {
//      for (k <- j to houses.length-1) {
//        println(j + "," + k + " " + matrix(j)(k).mkString(","))
//      }
//
//    }
    matrix(0)(houses.length-1)(mbCount)
  }

  def main(args: Array[String]): Unit = {
    println(minDistance(Array(2,5,7,10,14),2))
  }
}