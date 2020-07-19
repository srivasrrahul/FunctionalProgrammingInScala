import scala.collection.mutable

case class Id(val dayIndex : Int,val hasStock : Boolean,val buyPrice : Int,val numberOfPendingTransactions : Int)
object Solution {
  def maxProfit(k: Int, prices: Array[Int]): Int = {
    val cache = new mutable.HashMap[Id,Int]()
    def itr(dayIndex : Int,hasStock : Boolean,buyPrice : Int,numberOfTransactionsPending : Int) : Int = {
      if (numberOfTransactionsPending <= 0) {
        0
      }else {
        if (dayIndex == prices.length-1) {
          //last day
          if (hasStock == true) {
            //println("for day " + dayIndex + " " + (prices.last - buyPrice))
            math.max(prices.last - buyPrice,0)
          }else {
            0
          }
        }else {
          //if you have stock two options
          //sell it or wait
          //if you dont have two options
          //buy it or wait
          val id = new Id(dayIndex,hasStock,buyPrice,numberOfTransactionsPending)
          if (cache.contains(id)) {
            cache.get(id).get
          }else {
            if (hasStock == true) {
              val opt1 = (prices(dayIndex) - buyPrice) + itr(dayIndex + 1, false, 0, numberOfTransactionsPending - 1)
              val opt2 = itr(dayIndex + 1, hasStock, buyPrice, numberOfTransactionsPending)
              //println("For day " + dayIndex + " " + hasStock + " " + buyPrice + " " + scala.math.max(opt1,opt2))
              cache += ((id,scala.math.max(opt1, opt2)))
              scala.math.max(opt1, opt2)
            } else {
              val opt1 = itr(dayIndex + 1, true, prices(dayIndex), numberOfTransactionsPending - 1)
              val opt2 = itr(dayIndex + 1, hasStock, buyPrice, numberOfTransactionsPending)
              //println("For day " + dayIndex + " " + hasStock + " " + buyPrice + " " + scala.math.max(opt1,opt2))
              cache += ((id,scala.math.max(opt1, opt2)))
              scala.math.max(opt1, opt2)
            }
          }
        }
      }
    }

    if (prices.isEmpty == false) {
      itr(0,false,0,2*k)
    }else {
      0
    }

  }

  def main(args: Array[String]): Unit = {
    println(maxProfit(2,Array(3,3,5,0,0,3,1,4)))
  }
}