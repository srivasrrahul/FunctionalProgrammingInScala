import scala.collection.mutable.HashMap
case class PriceIndex(index : Int,priceBought : Int)

object Solution {
//  def maxProfit(prices: Array[Int]): Int = {
//    val cache = new HashMap[PriceIndex,Int]
//
//
//    def max(index : Int,priceBought : Int) : Int = {
//      if (index == prices.length-1) {
//        //Last index
//        if (priceBought > -1) {
//          //have already bought
//          if (prices(index) >= priceBought) {
//            prices(index) - priceBought
//          }else {
//            0
//          }
//        }else {
//          //No point in selling
//          0
//        }
//      }else {
//
//
//        val key = PriceIndex(index,priceBought)
//        cache.get(key) match {
//          case Some(p) =>  {
//            //println("cache hit")
//            p
//          }
//          case None => {
//            var maxPrice = Int.MinValue
//            //Case 1 -- buy today only if not bought already
//            if (prices(index) > 0 && priceBought == -1) {
//              val buyTodaySellLater = max(index + 1, prices(index))
//              if (buyTodaySellLater > maxPrice) {
//                maxPrice = buyTodaySellLater
//              }
//            }
//
//            //Case 2 -- If bought already sell today
//            if (priceBought > -1) {
//              if (prices(index) >= priceBought) {
//                val profitToday = prices(index) - priceBought
//                if (profitToday > maxPrice) {
//                  maxPrice = profitToday
//                }
//              }
//            }
//
//            //Case 3 -- do nothing and continue
//            val continueAsItIs = max(index+1,priceBought)
//            if (continueAsItIs > maxPrice) {
//              maxPrice = continueAsItIs
//            }
//
//            cache += (key -> maxPrice)
//            maxPrice
//          }
//        }
//
//      }
//    }
//
//    if (prices.length == 0) {
//      0
//    }else {
//      max(0, -1)
//    }
//  }


  def maxProfit(prices : Array[Int]) : Int = {
    var maxPrice = 0
    for (j <- 0 to prices.length-1) {
      val buyPrice = prices(j)
      for (k <- j+1 to prices.length-1) {
        if (prices(k) >= buyPrice) {
          val profit = prices(k) - buyPrice
          if (profit > maxPrice) {
            maxPrice = profit
          }
        }
      }
    }

    maxPrice
  }

  def main(args: Array[String]): Unit = {
    println(maxProfit(Array(7,6,4,3,1)))

  }
}