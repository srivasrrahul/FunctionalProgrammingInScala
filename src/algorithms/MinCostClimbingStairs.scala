import scala.collection.mutable.HashMap
import scala.math.min
case class Key(currentIndex : Int,costAlreadyPaid : Int,stepsFromLastCost : Int)
object Solution {

  def minCostClimbingStairs(cost : Array[Int]) : Int = {
    val costIncludingCurrent = new Array[Int](cost.length)
    val costUsingPast = new Array[Int](cost.length)

    costIncludingCurrent(0) = cost(0)
    //costUsingPast(0) = Int.MaxValue

    costIncludingCurrent(1) = cost(1)
    //costUsingPast(1) = costIncludingCurrent(0)

    for (j <- 2 to cost.length-1) {
      //If current included
      //min(Current(j) + current(j-1)) , (Current(j) + current(j-2)

      val option1 = cost(j) + costIncludingCurrent(j-1)
      val option2 = cost(j) + costIncludingCurrent(j-2)
      costIncludingCurrent(j) = min(option1,option2)

    }

    //println(costIncludingCurrent.mkString(","))


    min(costIncludingCurrent(cost.length-1),costIncludingCurrent(cost.length-2))


  }
//  def minCostClimbingStairs(cost: Array[Int]) : Int = {
//
//    val cache = new HashMap[Key,Int]
//    def process(currentIndex : Int,costPaidAlready : Int,stepsFromLastCost : Int) : Int = {
//
//      currentIndex match {
//        case x if x >= cost.length => {
//          costPaidAlready
//        }
//        case _ => {
//          //println("current Index " + currentIndex)
//          cache.get(Key(currentIndex, costPaidAlready, stepsFromLastCost)) match {
//            case Some(c) => {
//              //println("cache hit")
//              c
//            }
//            case None => {
//              var minCost = Int.MaxValue
//              stepsFromLastCost match {
//                case 1 => {
//                  //1. pay current as well and jump to next
//                  val totalCost = process(currentIndex + 1, costPaidAlready + cost(currentIndex), 1)
//                  if (totalCost < minCost) {
//                    minCost = totalCost
//                  }
//
//                  //2. pay current as well and jump to next 2
//                  val totalCost1 = process(currentIndex + 2, costPaidAlready + cost(currentIndex), 2)
//                  if (totalCost1 < minCost) {
//                    minCost = totalCost1
//                  }
//
//                  //3. don't pay current and jump to next
//                  val totalCost2 = process(currentIndex + 1, costPaidAlready, 2)
//                  if (totalCost2 < minCost) {
//                    minCost = totalCost2
//                  }
//                }
//                case 2 => {
//                  //You have to buy current one
//                  val totalCost = process(currentIndex + 1, costPaidAlready + cost(currentIndex), 1)
//                  if (totalCost < minCost) {
//                    minCost = totalCost
//                  }
//
//                }
//              }
//
//              cache += (Key(currentIndex, costPaidAlready, stepsFromLastCost) -> minCost)
//              minCost
//            }
//          }
//        }
//      }
//    }
//
//
//    var minCost = Int.MaxValue
//
//    val cost1 = process(1,cost(0),1)
//    if (cost1 < minCost) {
//      //println("1 => " + cost1)
//      minCost = cost1
//    }
//
//    val cost2 = process(2,cost(0),2)
//    if (cost2 < minCost) {
//      //println("2 => " + cost2)
//      minCost = cost2
//    }
//
//    //println("befor 3 => ")
//    val cost3 = process(2,cost(1),1)
//    if (cost3 < minCost) {
//      //println("3 => " + cost3)
//      minCost = cost3
//    }
//
//    val cost4 = process(3,cost(1),2)
//    if (cost4 < minCost) {
//      //println("4 => " + cost4)
//      minCost = cost4
//    }

    //minCost


  //}

  def main(args: Array[String]): Unit = {
    println(minCostClimbingStairs(Array(0,1,1,0)))
  }
}