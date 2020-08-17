import scala.collection.mutable

case class Index(val begin : Int,val end : Int,val pendingMailBox : Int)
object Solution {
  def minDistance(houses: Array[Int], k: Int): Int = {
    houses.sortInPlace()
    val cache = new mutable.HashMap[Index,Int]()
    def itr(begin : Int,end : Int,pendingMailBox : Int) : Int = {
      val index = new Index(begin, end, pendingMailBox)
      if (cache.contains(index)) {
        cache.get(index).get
      } else {
        //println(begin + " " + end + " " + pendingMailBox)
        if (pendingMailBox == 1) {
          val count = (end-begin+1)
          var midValue = 0
          if (count % 2 == 0) {
            val mid = begin + (end-begin)/2
            val mid1 = mid+1
            midValue = (houses(mid) + houses(mid+1))/2
          }else {
            val mid = begin + (end-begin)/2
            midValue = houses(mid)
          }

          var distance = 0
          for (j <- begin to end) {
            distance = distance + math.abs(houses(j)-midValue)
          }

          distance
        } else {
          //println("else " + begin + " " + end + " " + pendingMailBox)
          val count = end - begin + 1
          if (count <= pendingMailBox) {
            0
          } else {
            var minSum = Int.MaxValue
            for (j <- begin to end) {
              for (p <- 1 to pendingMailBox - 1) {
                val leftSum = itr(begin, j, p)
                var rightSum = 0
                if (j + 1 <= end) {
                  rightSum = itr(j + 1, end, pendingMailBox - p)
                }
                val totalSum = leftSum + rightSum
                if (totalSum < minSum) {
                  minSum = totalSum
                }
              }
            }

            //println("Result " + begin + " " + end + " " + pendingMailBox + " " + minSum)
            cache += ((index, minSum))
            minSum
          }
        }
      }
    }

    itr(0,houses.length-1,k)
  }

  def main(args: Array[String]): Unit = {
    println(minDistance(Array(3,6,14,10),3))
  }
}