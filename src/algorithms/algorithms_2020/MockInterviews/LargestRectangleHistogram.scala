import scala.collection.mutable.ArrayBuffer

object Solution {
  def largestRectangleArea(heights: Array[Int]): Int = {
    val areaLst = new ArrayBuffer[Int]()

    var maxArea = 0
    for (j <- 0 to heights.length-1) {
      for (k <- j to heights.length-1) {
        var minHeight = heights(j)
        for (m <- j+1 to k) {
          if (heights(m) < minHeight) {
            minHeight = heights(m)
          }


        }

        val area = minHeight*(k-j+1)
        if (area > maxArea) {
          maxArea = area
        }
      }
    }

    maxArea
  }
}