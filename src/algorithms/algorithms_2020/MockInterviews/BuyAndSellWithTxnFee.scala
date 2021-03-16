import scala.collection.mutable

object Solution {
  def maxProfit(prices: Array[Int], fee: Int): Int = {
    val cache = new mutable.HashMap[Int,Int]()
    def itr(k : Int) : Int = {
      if (k <= 0) {
        0
      }else {
        //sell today and buy earlier
        if (cache.contains(k)) {
          cache.get(k).get
        }else {
          var maxProfit = itr(k - 1)
          for (j <- k - 1 to 0 by -1) {
            if (prices(j) < prices(k)) {
              val currProfit = prices(k) - prices(j) - fee + itr(j - 1)
              if (currProfit > maxProfit) {
                maxProfit = currProfit
              }
            }
          }

          cache += ((k,maxProfit))
          maxProfit
        }

      }
    }

    itr(prices.length-1)
  }

  def main(args: Array[String]): Unit = {
    println(maxProfit(Array(1,3,2,8,4,9),2))
  }
}