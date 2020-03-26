object Solution {
  def rob(nums: Array[Int]): Int = {
    nums.length match {
      case 0 => {
        0
      }
      case 1 => nums(0)
      case 2 => scala.math.max(nums(0),nums(1))
      case _ => {
        val maxAmount = new Array[Int](nums.length)
        maxAmount(0) = nums(0)
        maxAmount(1) = scala.math.max(nums(0),nums(1))

        for (j <- 2 to nums.length-1) {
          val option1 = nums(j) + maxAmount(j-2)
          val option2 = maxAmount(j-1)

          maxAmount(j) = scala.math.max(option1,option2)
        }

        maxAmount.max
      }
    }

  }

  def main(args: Array[String]): Unit = {
    println(rob(Array(2)))
  }
}