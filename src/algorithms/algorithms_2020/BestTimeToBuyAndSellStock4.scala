object Solution {
  def maxProfit(k: Int, prices: Array[Int]): Int = {
    def itr(dayIndex : Int,hasStock : Boolean,buyPrice : Int) : Int = {
      if (dayIndex == prices.length-1) {
        if (hasStock == true) {
          if (prices.last > buyPrice) {
            prices.last - buyPrice
          }else {
            0
          }
        }else {
          0
        }
      }else {
        //if you have stock
        //two optiosn a) Sell it b) Wait
        //if you don't have stcok
        //two options a) Buy it b) Wait
        if (hasStock == true) {
          val opt1 = (prices(dayIndex) - buyPrice) + itr(dayIndex+1,false,0)
          val opt2 = itr(dayIndex+1,hasStock,buyPrice)
          scala.math.max(opt1,opt2)
        }else {
          val opt1 = itr(dayIndex+1,true,prices(dayIndex))
          val opt2 = itr(dayIndex+1,false,0)
          scala.math.max(opt1,opt2)
        }
      }
    }

    itr(0,false,0)
  }

  def main(args: Array[String]): Unit = {
    println(maxProfit(2,Array(3,2,6,5,0,3)))
  }
}