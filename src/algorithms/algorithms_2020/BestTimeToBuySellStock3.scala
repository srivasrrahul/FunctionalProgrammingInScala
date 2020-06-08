object Solution {
  def maxProfit(prices: Array[Int]): Int = {
    if (prices.isEmpty) {
      0
    }else {
      val matrix = Array.ofDim[Int](3, prices.length)
      val maxProfitForSingleSell = new Array[Int](prices.length)


      //row 0 if I don't indulge in any transaction till date
      for (j <- 0 to matrix(0).length - 1) {
        matrix(0)(j) = 0
      }

      //row 1 if I only engage in one transaction till date and sell on day j
      for (j <- 1 to matrix(0).length - 1) {
        val current = prices(j)
        var maxProfitToday = Int.MinValue
        for (k <- j - 1 to 0 by -1) {
          var profit = current - prices(k)
          if (profit > maxProfitToday) {
            maxProfitToday = profit
          }


        }

        matrix(1)(j) = maxProfitToday
        if (j == 1 ) {
          maxProfitForSingleSell(1) = matrix(1)(j)
        }else {
          maxProfitForSingleSell(j) = scala.math.max(maxProfitForSingleSell(j-1),matrix(1)(j))
        }
      }

      //row 2 if I engage in two transactions till date and sell last today
      //option 1 => current - prices(k) + max(matrix(2)(k-1) //have sold one today and rest I shud sell earlier
      for (j <- 3 to matrix(2).length - 1) {
        val current = prices(j)
        var maxProfitForJ = Int.MinValue
        //println("In " + j)
        for (k <- j - 1 to 2 by -1) {
//          var maxProfitSingleSellForK = Int.MinValue
//          for (l <- 1 to k - 1) {
//            if (matrix(1)(l) > maxProfitSingleSellForK) {
//              maxProfitSingleSellForK = matrix(1)(l)
//            }
//          }

          //val currentProfit = maxProfitSingleSellForK + (current - prices(k)) //max prev to j + (current - buyatj)
          val currentProfit = (current - prices(k)) + maxProfitForSingleSell(k-1)
          //println("For " + j + " " + currentProfit)
          if (currentProfit > maxProfitForJ) {
            maxProfitForJ = currentProfit
          }

        }

        matrix(2)(j) = maxProfitForJ

        //two options
        //option 1 => current - prices(k) + max(matrix(1)(k-1) //have sold one today and rest I shud sell earlier
        //option 2 => current - prices(k) + no negative
      }

//      println(matrix(1).mkString(","))
//      println(matrix(2).mkString(","))
      Array(matrix(0).max, matrix(1).max, matrix(2).max).max
    }

  }



  def main(args: Array[String]): Unit = {
    println(maxProfit(Array(3,3,5,0,0,3,1,4)))
  }

}