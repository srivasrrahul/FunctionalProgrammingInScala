import scala.collection.mutable

object Solution {
  def findErrorNums(nums: Array[Int]): Array[Int] = {
    val n = nums.length
    //val mapCount = new mutable.HashSet[Int]()

    //1,2,2,4 = 7

    val bitSet = new mutable.BitSet()
    var repeatedNumber = 0
    for (num <- nums ) {
      if (bitSet.contains(num)) {
        repeatedNumber = num
      }else {
        bitSet.add(num)
      }

    }

    val missingNumber = (n*(n+1))/2 - (nums.sum - repeatedNumber)

    Array(repeatedNumber,missingNumber)


  }
}