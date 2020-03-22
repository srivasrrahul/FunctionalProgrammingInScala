object Solution {
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.length == 0) {
      0
    }else {
      val profitArr = new Array[Int](prices.size)
      profitArr(0) = 0

      for (j <- 1 to prices.length - 1) {
        //println("Day = " + (j+1) + " selling price = " + prices(j))
        val sellingPrice = prices(j)
        var profit = Int.MinValue
        for (k <- j - 1 to 0 by -1) {
          if (sellingPrice > prices(k)) {
            val profitCurrent = sellingPrice - prices(k)
            if (profitCurrent > profit) {
              profit = profitCurrent
            }
          }
        }


        //println(profit)
        profitArr(j) = profit
      }

      profitArr.max
    }
  }

  def main(args: Array[String]): Unit = {
    println(maxProfit(Array(1)))
  }
}