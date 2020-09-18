import scala.collection.mutable

object Solution {
  def maxJumps(arr: Array[Int], D: Int): Int = {
    def allHeigtBetween(f : Int,t : Int,upperLimit : Int) : Boolean = {
      var lessThan = true
      for (j <- f to t if lessThan == true) {
        if (arr(j) >= upperLimit) {
          lessThan = false
        }
      }

      lessThan
    }

    val cache = new mutable.HashMap[Int,Int]()
    def itr(j : Int) : Int = {
      if (cache.contains(j)) {
        //println("Cache hit")
        cache.get(j).get
      }else {

        var maxTouchPoint = 1
        for (k <- j + 1 to j + D if k < arr.length) {
          if (allHeigtBetween(j+1, k, arr(j))) {
            val touchPoint = 1 + itr(k)
            if (touchPoint > maxTouchPoint) {
              maxTouchPoint = touchPoint
            }
          }
        }

        for (k <- j - 1 to j - D by -1 if k >= 0) {
          // if (j == 10) {
          //     println(k + " " + allHeigtBetween(k, j-1, arr(j)))
          // }
          if (allHeigtBetween(k, j-1, arr(j))) {
            val touchPoint = 1 + itr(k)
            if (touchPoint > maxTouchPoint) {
              maxTouchPoint = touchPoint
            }
          }
        }

        cache += ((j,maxTouchPoint))
        maxTouchPoint
      }
    }

    var maxPoints = 0
    for (j <- 0 to arr.length-1) {
      val points = itr(j)
      if (points > maxPoints) {
        maxPoints = points
      }
    }

    //println(cache)
    maxPoints
  }

  def main(args: Array[String]): Unit = {
    println(maxJumps(Array(7,1,7,1,7,1),2))
  }
}