object Solution {
  def minSubArrayLen(s: Int, nums: Array[Int]): Int = {
    var begin = 0
    var end = 0
    var minLength = Int.MaxValue

    var sum = 0
    while (end < nums.length) {
      //println(end)

      while (sum < s && end < nums.length) {
        sum = sum + nums(end)
        end = end + 1
      }

      while (sum >= s && begin < nums.length) {
        val len = end - begin
        if (len < minLength) {
          //println("end " + end + " begin " + begin)
          minLength = len
        }

        sum = sum - nums(begin)
        begin = begin + 1
      }

    }

    if (minLength == Int.MaxValue) {
      0
    }else {
      minLength
    }

  }
}