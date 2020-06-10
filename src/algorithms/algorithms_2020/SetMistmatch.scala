import scala.collection.mutable

object Solution {
  def findErrorNums(nums: Array[Int]): Array[Int] = {
    val n = nums.length
    val mapCount = new mutable.HashMap[Int,Int]()

    //1,2,2,4 = 7

    var repeatedNumber = 0
    for (num <- nums) {
      val defaultCount = mapCount.getOrElseUpdate(num,0)
      if (defaultCount == 1) {
        repeatedNumber = num
      }

    }

    val missingNumber = (n*(n+1))/2 - (nums.sum - repeatedNumber)

    Array(repeatedNumber,missingNumber)


  }
}