object Solution {
  def maxArea(height: Array[Int]): Int = {
    val container = new Array[Int](height.length)
    val delta = 1
    container(0) = 0
    container(1) = scala.math.min(height(0),height(1)) * (1-0)*delta

    for (j <- 2 to height.length-1) {
      var maxContainerSize = Int.MinValue
      for (k <- j-1 to 0 by -1) {

        val conatinerSize = scala.math.min(height(j),height(k)) * (j-k)*delta
        //println("For j k " + j + "  " + k + " size " + conatinerSize)
        if (conatinerSize > maxContainerSize) {
          maxContainerSize = conatinerSize
        }

        container(j) = maxContainerSize
      }
    }

    container.max
  }

  def main(args: Array[String]): Unit = {
    println(maxArea(Array(1,8,6,2,5,4,8,3,7)))
  }
}