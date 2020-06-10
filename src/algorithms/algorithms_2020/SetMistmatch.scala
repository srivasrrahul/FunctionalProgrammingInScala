import scala.collection.mutable

object Solution {
  def findErrorNums(nums: Array[Int]): Array[Int] = {
    val n = nums.length
    val mapCount = new mutable.HashSet[Int]()

    //1,2,2,4 = 7

    var repeatedNumber = 0
    for (num <- nums if repeatedNumber == 0) {
      if (mapCount.contains(num)) {
        repeatedNumber = num
      }else {
        mapCount.add(num)
      }
    }

    val missingNumber = (n*(n+1))/2 - (nums.sum - repeatedNumber)

    Array(repeatedNumber,missingNumber)


  }
}