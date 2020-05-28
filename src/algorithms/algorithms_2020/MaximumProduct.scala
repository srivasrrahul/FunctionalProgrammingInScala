object Solution {
  def maximumProduct(nums: Array[Int]): Int = {

    //[-1,-3,2,3]
    //-3,-1,2,3
    //-5,-4,1,20,10
    if (nums.length == 3) {
      nums(0)*nums(1)*nums(2)
    }else {
      nums.sortInPlace()
      //println(nums.mkString(","))

      val firstTwo = nums.head * nums(1)

      val lastTwo = nums.last * nums(nums.length-2)

      val option1 = nums.last * nums(nums.length-2) * nums(nums.length-3)

      val option2 = nums.last * nums.head * nums(1)

      scala.math.max(option1,option2)





    }
  }

  def main(args: Array[String]): Unit = {
    println(maximumProduct(Array(-1,-3,2,3)))
  }
}