import scala.collection.mutable

object Solution {
  def maximumGap(nums: Array[Int]): Int = {
    if (nums.isEmpty == true) {
      0
    }else {
      val set = new mutable.HashSet[Int]()
      for (num <- nums) {
        set.add(num)
      }

      val arr = set.toArray.sortInPlace()
      var prev = arr(0)
      var maxDiff = 0
      for (num <- arr.tail) {
        val diff = num-prev
        if (diff > maxDiff) {
          maxDiff = diff
        }

        prev = num
      }

      maxDiff
    }
  }
}