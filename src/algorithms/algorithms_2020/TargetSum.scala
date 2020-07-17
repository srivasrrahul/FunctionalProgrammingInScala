object Solution {
  def findTargetSumWays(nums: Array[Int], S: Int): Int = {
    var count = 0
    def itr(index : Int,accumulatedSum : Int) : Unit = {
      if (index == nums.length) {
        if (accumulatedSum == S) {
          count = count + 1
        }
      }else {
        itr(index+1,accumulatedSum+(nums(index)))
        itr(index+1,accumulatedSum-(nums(index)))
      }
    }

    itr(0,0)
    count
  }

  def main(args: Array[String]): Unit = {
    println(findTargetSumWays(Array(1, 1, 1, 1, 1),3))
  }
}