object Solution {
  def longestConsecutive(nums: Array[Int]): Int = {
    if (nums.length == 0) {
      0
    }else {
      //0,1,1,2
      nums.sortInPlace()
      //val lcs = new Array[Int](nums.length)
      //lcs(0) = 1
      //0,1,1,2
      //1,2

      var increments = 1
      var maxIncrements = 1
      for (j <- 1 to nums.length - 1) {
        var maxCount = 1
        var breakIfNotFound = false

        if (nums(j) == nums(j-1)+1) {
          increments = increments + 1
          if (increments > maxIncrements) {
            maxIncrements = increments
          }
        }else {
          //bad lets restart
          if (nums(j) == nums(j-1)) {
            //ignore current if eqyal
          }else {
            increments = 1
          }
        }

        //1,2,3,4,100,200
        // for (k <- j - 1 to 0 by -1) {
        //   if (nums(k) == nums(j) - 1) {
        //     val currentCount = 1 + lcs(k)
        //     if (currentCount > maxCount) {
        //       maxCount = currentCount
        //     }
        //   }else {
        //      //breakIfNotFound = true
        //   }
        // }
        //0,1,1,2
        //1,2,


      }

      //println(lcs.mkString(","))
      maxIncrements
    }


  }

  def main(args: Array[String]): Unit = {
    //0,1,1,2
    println(longestConsecutive(Array(1,2,0,1)))
  }


}