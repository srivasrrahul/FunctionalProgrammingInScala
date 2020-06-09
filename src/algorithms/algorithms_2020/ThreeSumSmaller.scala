import scala.collection.mutable

object Solution {
  def threeSumSmaller(nums: Array[Int], target: Int): Int = {
    if (nums.length < 3) {
      0
    }else {
      nums.sortInPlace()

      //-2,0,0,2,2
      def twoSum(index: Int, sumPending: Int): Int = {
        //Do it on order of n
        //0,1,3    4
        //0+3 < 4 implies 0+1 is also true

        var current = index
        var last = nums.length - 1

        var count = 0
        while (current < last) {
          if (nums(current) + nums(last) >= sumPending) {
            last = last - 1
          } else {
            count = count + (last-current)
            current = current + 1

          }
        }

        count
      }

      var result = 0
      for (j <- 0 to nums.length - 3) {
        val pendingSum = target - nums(j)
        val countPending = twoSum(j + 1, pendingSum)
        result = result + countPending
      }

//      if (nums(0) + nums(1) + nums(2) < target) {
//        result = result + 1
//      }

      result
    }


  }

  def main(args: Array[String]): Unit = {
    //println(threeSumSmaller(Array(-2,0,1,3),2))
    println(threeSumSmaller(Array(-2,1,1),2))
  }
}