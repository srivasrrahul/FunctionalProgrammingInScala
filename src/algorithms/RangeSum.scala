class NumArray(_nums: Array[Int]) {

  val cache = new Array[Int](_nums.length)
  init()
  def init() : Unit = {
    cache.length match {
      case 0 => {

      }
      case 1 => {
        cache(0) = 0
      }
      case _ => {
        cache(0) = 0
        var sumVal = _nums(0)
        for (j <- 1 to cache.length-1) {
          cache(j) = sumVal
          sumVal = sumVal + _nums(j)
        }
      }
    }


  }
  def sumRange(i: Int, j: Int): Int = {
    cache(j) - cache(i) + _nums(j)

  }



}

object Solution {
  def main(args: Array[String]): Unit = {
    val numArray = new NumArray(Array(-2, 0, 3, -5, 2, -1))
    println(numArray.sumRange(0,5))

  }
}