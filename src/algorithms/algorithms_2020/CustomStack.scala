class CustomStack(val _maxSize: Int) {

  val arr = new Array[Int](_maxSize)
  var top = -1
  def push(x: Int) : Unit = {
    if (top < arr.length-1) {
      top = top + 1
      arr(top) = x
    }
  }

  def pop(): Int = {
    if (top < 0) {
      -1
    }else {
      val topElement = arr(top)
      top = top - 1
      topElement
    }

  }

  def increment(k: Int, incCount : Int) : Unit = {
    for (j <- 0 to scala.math.min(k-1,top)) {
      arr(j) = arr(j) + incCount
    }
  }

}

/**
 * Your CustomStack object will be instantiated and called as such:
 * var obj = new CustomStack(maxSize)
 * obj.push(x)
 * var param_2 = obj.pop()
 * obj.increment(k,`val`)
 */