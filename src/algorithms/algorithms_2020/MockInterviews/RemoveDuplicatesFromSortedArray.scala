object Solution {
  def removeDuplicates(nums: Array[Int]): Int = {
    if (nums.length == 0) {
      0
    }else {
      //1,1,1,1,2,2,2,2,3,3
      //1,1,2,2,2,2,3,3
      //moveLeft(4,2) //1,1,2
      //1,1,2,2,2,2,3,3
      //1,1,1,2,2,3
      //1,1,2,2,3

      var reducedLen = 0
      var maxIndex = nums.length-1
      //println(maxIndex)
      def shiftLeft(left : Int,right : Int) : Unit = {
        var k = right
        for (j <- left to maxIndex if k <= maxIndex) {
          nums(j) = nums(k)
          k = k+1
        }
      }

      var j = 0
      while (j <= maxIndex) {
        var k = j
        while (k <= maxIndex && nums(j) == nums(k)) {
          k = k + 1
        }

        if (k-j > 2) {
          shiftLeft(j+2,k)
          reducedLen = reducedLen + (k-j-2)
          maxIndex = maxIndex-(k-j-2)
          //println(nums(j) + " changed maxIndex to " + maxIndex + " because " + (k-j-2))
          j = j+2
        }else {
          if (j == k) {
            j = j+1
          }else {
            j = k
          }
        }



      }

      //println(nums.mkString(","))
      maxIndex+1
    }
  }
}