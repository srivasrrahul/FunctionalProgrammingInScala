import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Matrix(val rows : Int,val cols : Int)


object Solution {


  def maxCoins(nums: Array[Int]): Int = {
    if (nums.isEmpty) {
      0
    }else {

      val coinsCache = new mutable.HashMap[List[Int], Int]()

      def coinsItr(lst: List[Int], tab: String): Int = {


        if (lst.length == 1) {
          lst.head
        } else {
          if (coinsCache.contains(lst)) {
            //println("cache hit")
            coinsCache.get(lst).get
          } else {
            var maxLocalCost = Int.MinValue
            var itr = lst
            val prevListBuffer = new ListBuffer[Int]
            while (itr != Nil) {
              //break this

              var last = 1
              if (prevListBuffer.size > 0) {
                last = prevListBuffer.last
              }


              var next = 1
              if (itr.tail != Nil) {
                next = itr.tail.head
              }

              val nextCost = coinsItr(prevListBuffer.toList ++ itr.tail, tab ++ " ")

              val currentCost = nextCost + next * itr.head * last

              if (currentCost > maxLocalCost) {
                maxLocalCost = currentCost
              }

              prevListBuffer.append(itr.head)

              itr = itr.tail

            }

            coinsCache += ((lst, maxLocalCost))
            //println(tab + " " + lst + " => " + maxLocalCost)
            maxLocalCost
          }
        }
      }

      coinsItr(nums.toList, "")
    }


  }

  def main(args: Array[String]): Unit = {
    println(maxCoins(Array(8,3,4,3,5,0,5,6,6,2,8,5,6,2,3,8,3,5,1,0,2)))
  }
}