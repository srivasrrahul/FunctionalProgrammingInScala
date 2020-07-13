import scala.collection.mutable

object Solution {
  def findDisappearedNumbers(nums: Array[Int]): List[Int] = {
    val n = nums.length
    val bitSet = new mutable.HashSet[Int]()
    for (j <- 1 to n) {
      bitSet.add(j)
    }

    for (num <- nums) {
      bitSet.remove(num)
    }

    val lst = bitSet.toList
    lst
  }

  def main(args: Array[String]): Unit = {
    println(findDisappearedNumbers(Array(4,3,2,7,8,2,3,1)))
  }
}