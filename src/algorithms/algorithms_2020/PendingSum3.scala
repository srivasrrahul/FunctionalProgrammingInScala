import scala.collection.mutable

object Solution {
  def combinationSum4(nums: Array[Int], target: Int): Int = {
    //if target is 0 what happens?
    //pendingSum(pendingTarget) : for all in nums if nums < pendingTarget pendingSum(pendingTarget - nums[j]) : add current element and return
    //also for ordering perspective helpful to sort this
    //its positive right because negative doesn't seem to have any end
    //1,2,3 4   => 1,1,1,1 1,3. 2,2 4
    //itr(4) => 1 + itr(3)
    //itr(3) => 1 + itr(2) 2 + itr(1) [[3]]
    //itr(2) => 1 + itr(1) [[2]]
    //itr(1) => [[1]]

    //nums.sortInPlace()
    val cache = new mutable.HashMap[Int,Option[Int]]()
    def pendingSum(pendingCount : Int) : Option[Int] = {
      //println(pendingCount)
      cache.get(pendingCount) match {
        case Some(existingVal) => existingVal
        case _ => {
          var count = 0
          for (num <- nums) {
            val diff = pendingCount - num
            diff match {
              case 0 => {
                count = count + 1
              }
              case negative if diff < 0 => {

              }
              case _ => {
                pendingSum(diff) match {
                  case None => {

                  }
                  case Some(createdCount) => {
                    count = count + createdCount
                  }
                }
              }
            }
          }

          if (count > 0) {
            cache += ((pendingCount,Some(count)))
            //println("pendingCount " + count)
            Some(count)
          }else {
            cache += ((pendingCount,None))
            None
          }

        }
      }
    }

    pendingSum(target) match {
      case None => 0
      case Some(count) => count
    }
  }



  def main(args: Array[String]): Unit = {
    println(combinationSum4(Array(1,2,3),4))
  }


}