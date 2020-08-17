object Solution {
  def minDistance(houses: Array[Int], k: Int): Int = {
    def itr(begin : Int,end : Int,pendingMailBox : Int) : Int = {
      println(begin + " " + end + " " + pendingMailBox)
      if (pendingMailBox == 1) {
        var totalDistance = 0
        for (j <- begin to end) {
          totalDistance = totalDistance + houses(j)
        }

        val centroid = totalDistance/(end-begin+1)
        //println("Centroid is " + centroid)

        var distance = 0
        for (j <- begin to end) {
          distance = distance + math.abs(houses(j)-centroid)
        }

        distance
      }else {
        //println("else " + begin + " " + end + " " + pendingMailBox)
        val count = end-begin+1
        if (count <= pendingMailBox) {
          0
        } else {
          var minSum = Int.MaxValue
          for (j <- begin to end) {
            for (p <- 1 to pendingMailBox - 1) {
              val leftSum = itr(begin, j, p)
              var rightSum = 0
              if (j+1 <= end) {
                rightSum = itr(j + 1, end, pendingMailBox - p)
              }
              val totalSum = leftSum + rightSum
              if (totalSum < minSum) {
                minSum = totalSum
              }
            }
          }

          //println("Result " + begin + " " + end + " " + pendingMailBox + " " + minSum)
          minSum
        }
      }
    }

    itr(0,houses.length-1,k)
  }

  def main(args: Array[String]): Unit = {
    println(minDistance(Array(3,6,14,10),4))
  }
}