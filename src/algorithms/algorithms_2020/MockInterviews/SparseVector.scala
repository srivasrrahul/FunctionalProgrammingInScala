import scala.collection.mutable

class SparseVector(val nums: Array[Int]) {
  // Return the dotProduct of two sparse vectors
  val nonZeroSet = new mutable.HashSet[Int]
  for (j <- 0 to nums.length-1) {
    if (nums(j) != 0) {
      nonZeroSet.add(j)
    }
  }

  def dotProduct(vec: SparseVector): Int = {
    var sum = 0
    for (baseIndex <- nonZeroSet) {
      val _nums = vec.nums
      sum = sum + nums(baseIndex)*_nums(baseIndex)
    }

    sum
  }
}