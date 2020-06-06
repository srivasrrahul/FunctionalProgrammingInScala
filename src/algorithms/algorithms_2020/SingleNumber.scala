object Solution {
  def singleNumber(nums: Array[Int]): Int = {
    var res = 0
    for (num <- nums) {
      res = res ^ num
    }

    res

  }

  def main(args: Array[String]): Unit = {

    println(singleNumber(Array(4,1,2,1,2)))
  }
}