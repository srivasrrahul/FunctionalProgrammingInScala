  object Solution {
    def findUnsortedSubarray(nums: Array[Int]): Int = {
      var sortedArr = nums.clone().sortInPlace()
      var leftMost = Int.MaxValue
      var rightMost = Int.MinValue

      for (j <- 0 to nums.length-1 if leftMost == Int.MaxValue) {
        if (sortedArr(j) != nums(j)) {
          leftMost = j
        }
      }

      for (j <- nums.length-1 to 0 by -1 if rightMost == Int.MinValue) {
        if (sortedArr(j) != nums(j)) {
          rightMost = j
        }
      }

      if (leftMost != Int.MaxValue) {
        rightMost-leftMost+1
      }else {
        0
      }

    }

    def main(args: Array[String]): Unit = {
      println(findUnsortedSubarray(Array(2, 6, 4, 8, 10, 9, 15)))
    }
  }