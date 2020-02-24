object Solution {

  def lengthOfLIS(nums : Array[Int]) : Int = {
    if (nums.length < 1) {
      0
    }else {
      val cache = Array.ofDim[Int](nums.length)
      cache(0) = 1

      for (j <- 1 to nums.length - 1) {
        var max_val = 1
        for (k <- j - 1 to 0 by -1) {

          if (nums(k) < nums(j)) {
            val new_length = cache(k) + 1
            if (new_length > max_val) {
              max_val = new_length
            }
          }
        }

        cache(j) = max_val
      }

      cache.max
    }
  }
//  def lengthOfLIS(nums: Array[Int]): Int = {
//    def itr(index : Int,count_till_now : Int,min_value : Int) : Int  = {
//      if (index == nums.length-1) {
//        if (nums(index) > min_value) {
//          count_till_now+1
//        }else {
//          count_till_now
//        }
//      }else {
//        //three options
//        //begin from current
//        var longest_length = Int.MinValue
//        val option1 = itr(index+1,1,nums(index))
//        if (option1 > longest_length) {
//          longest_length = option1
//        }
//
//        //drop current
//        val option2 = itr(index+1,count_till_now,min_value)
//        if (option2 > longest_length) {
//          longest_length = option2
//        }
//
//        //use current if possible in conjuction with previous
//        if (nums(index) > min_value) {
//          val option3 = itr(index+1,count_till_now+1,nums(index))
//          if (option3 > longest_length) {
//            longest_length = option3
//          }
//        }
//
//        longest_length
//      }
//    }
//
//    if (nums.length == 0) {
//      0
//    }else {
//      itr(0,0,Int.MinValue)
//    }
//
//  }

  def main(args: Array[String]): Unit = {
    //println(lengthOfLIS(Array(10,9,2,5,3,7,101,18)))
    println(lengthOfLIS(Array(1,2)))
  }
}