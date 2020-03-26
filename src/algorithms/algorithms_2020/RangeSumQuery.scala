class NumArray(_nums: Array[Int]) {

  val sumArr = new Array[Int](_nums.size)
  if (_nums.size > 0) {
    sumArr(0) = _nums(0)

    for (j <- 1 to _nums.length-1) {
      sumArr(j) = sumArr(j-1) + _nums(j)
    }
  }


  //println(sumArr.mkString(","))
  def sumRange(i: Int, j: Int): Int = {
    val begin = sumArr(i)
    val end = sumArr(j)

    end-begin + _nums(i)
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val numArr = new NumArray(Array())
    //println(numArr.sumRange(0,0))
  }
}