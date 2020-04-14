object Solution {
  def subarraySum(nums: Array[Int], checkValue: Int): Int = {
    val sumArr = new Array[Int](nums.length)
    sumArr(0) = nums(0)
    for (j <- 1 to nums.length-1) {
      sumArr(j) = sumArr(j-1) + nums(j)
    }

    var count = 0
    for (j <- 0 to nums.length-1) {
      for (k <- j to nums.length-1) {
        //Get sum frm j to k
        val sumjk = sumArr(k) - sumArr(j) + nums(j)
        if (sumjk == checkValue) {
          count += 1
        }
      }
    }

    count
  }

  def main(args: Array[String]): Unit = {
    println(subarraySum(Array(1,1,1),2))
  }
}