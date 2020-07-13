import scala.collection.mutable

object Solution {
  def numIdenticalPairs(nums: Array[Int]): Int = {
    val map = new mutable.HashMap[Int,Int]()
    var goodPairs = 0
    for (j <- 0 to nums.length-1) {
      val earlierCount = map.getOrElse(nums(j),0)
      if (earlierCount > 0) {
        goodPairs = goodPairs + earlierCount
      }
      map += ((nums(j),earlierCount+1))
    }

    goodPairs
  }

  def main(args: Array[String]): Unit = {
    println(numIdenticalPairs(Array(1,2,3)))
  }
}