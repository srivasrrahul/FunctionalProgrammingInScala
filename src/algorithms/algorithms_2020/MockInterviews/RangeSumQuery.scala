class NumArray(_nums: Array[Int]) {

  val sums = new Array[Int](_nums.length)
  if (_nums.length > 0) {
    sums(0) = _nums(0)
  }

  for (j <- 1 to _nums.length-1) {
    sums(j) = sums(j-1) + _nums(j)
  }

  //println(sums.mkString(","))

  def update(i: Int, v: Int)  : Unit = {
    val old = _nums(i)
    _nums(i) = v

    val delta = v - old
    //reset sums from i to end
    for (j <- i to sums.length-1) {
      sums(j) = sums(j) + delta
    }
  }

  def sumRange(i: Int, j: Int): Int = {
    var total = sums(j)
    if (i > 0) {
      total = total-sums(i-1)
    }

    total
  }

}

/**
 * Your NumArray object will be instantiated and called as such:
 * var obj = new NumArray(nums)
 * obj.update(i,`val`)
 * var param_2 = obj.sumRange(i,j)
 */