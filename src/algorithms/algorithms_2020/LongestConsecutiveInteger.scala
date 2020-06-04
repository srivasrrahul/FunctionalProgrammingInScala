object Solution {
  def longestConsecutive(nums: Array[Int]): Int = {
    if (nums.length == 0) {
      0
    }else {
      //0,1,1,2
      nums.sortInPlace()
      val lcs = new Array[Int](nums.length)
      lcs(0) = 1

      for (j <- 1 to nums.length - 1) {
        var maxCount = 1
        for (k <- j - 1 to 0 by -1) {
          if (nums(k) == nums(j) - 1) {
            val currentCount = 1 + lcs(k)
            if (currentCount > maxCount) {
              maxCount = currentCount
            }
          }
        }

        lcs(j) = maxCount
      }

      //println(lcs.mkString(","))
      lcs.max
    }


  }

  def main(args: Array[String]): Unit = {
    //0,1,1,2
    //1,1,
    println(longestConsecutive(Array(1,2,0,1)))
  }


}