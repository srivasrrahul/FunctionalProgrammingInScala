import scala.collection.mutable
import util.control.Breaks._
object Solution {
  def coinChange(coins: Array[Int], amount: Int): Int = {
    if (amount == 0) {
      0
    }else {
      val pendingAmountCache = new mutable.HashMap[Int, Int]()

      def itr(pendingAmount: Int): Int = {
        if (pendingAmountCache.contains(pendingAmount)) {
          pendingAmountCache.get(pendingAmount).get
        } else {
          var minCount = -1
          breakable {
            coins.foreach(coinValue => {
              if (coinValue == pendingAmount) {
                minCount = 1
                break()
              }
            })
          }

          minCount match {
            case 1 => {
              1
            }
            case _ => {
              coins.foreach(coinValue => {
                if (pendingAmount >= coinValue) {
                  val takeCurrentCount = itr(pendingAmount - coinValue)
                  if (takeCurrentCount != -1) {
                    //Its possible
                    val netCount = 1 + takeCurrentCount
                    if (minCount == -1) {
                      minCount = netCount
                    } else {
                      if (netCount < minCount) {
                        minCount = netCount
                      }
                    }
                  }
                }
              })

              pendingAmountCache += ((pendingAmount, minCount))
              minCount
            }
          }
        }

      }

      itr(amount)
    }
  }

//  def coinChange(coins: Array[Int], amount: Int) : Int = {
//    if (amount == 0) {
//     1
//    }else {
//      val changePossible = new Array[Int](amount)
//
//      for (j <- 0 to changePossible.length - 1) {
//        changePossible(j) = -1
//      }
//
//      coins.foreach(coin => {
//        if (coin < changePossible.length) {
//          changePossible(coin) = 1
//        }
//      })
//
//      val maxCoinValue  = coins.max
//
//
//      for (coinCurrentValue <- maxCoinValue to amount-1) {
//        var changeMinValue = -1
//        for (coinValue <- coins) {
//          val pendingValue = coinCurrentValue - coinValue
//
//          if (changePossible(pendingValue) != -1) {
//            if (changeMinValue == -1) {
//              changeMinValue = 1 + changePossible(pendingValue)
//            } else {
//              //relaxation
//              if ((1 + changePossible(pendingValue)) < changeMinValue) {
//                changeMinValue = 1 + changePossible(pendingValue)
//              }
//            }
//          }
//        }
//
//        changePossible(coinCurrentValue) = changeMinValue
//      }
//
//      //println(changePossible.mkString(","))
//
//      changePossible(amount-1)
//
//    }
//  }

  def main(args: Array[String]): Unit = {
    println(coinChange(Array(1,2,5),11))
  }
}