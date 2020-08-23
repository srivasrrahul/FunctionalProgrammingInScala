import scala.collection.mutable

object Solution {
  def coinChange(coins: Array[Int], totalAmount: Int): Int = {
    val cache = new mutable.HashMap[Int,Int]()
    def itr(amount : Int) : Int = {
      if (amount == 0) {
        0
      }else {
        if (cache.contains(amount)) {
          cache.get(amount).get
        }else {
          var minAmount = Int.MaxValue
          for (coin <- coins) {
            val pendingAmount = amount - coin
            if (pendingAmount == 0) {
              minAmount = 1
            } else {
              if (pendingAmount > 0) {
                val amounts = itr(pendingAmount)
                if (amounts != Int.MaxValue) {
                  if (amounts + 1 < minAmount) {
                    minAmount = amounts + 1
                  }
                }
              }
            }
          }

          cache += ((amount,minAmount))
          minAmount
        }
      }
    }

    if (coins.isEmpty) {
      -1
    }else {
      val retValue = itr(totalAmount)
      if (retValue == Int.MaxValue) {
        -1
      } else {
        retValue
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(coinChange(Array(1,2,5),11))
  }
}