import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def findSpecialInteger(arr: Array[Int]): Int = {
    var count = 1
    var special = arr(0)
    var found = false
    for (j <- 1 to arr.length-1 if found == false) {
      if (arr(j) != arr(j-1)) {
        val percentage = count.toFloat/arr.length.toFloat
        //println("percentahe for " + arr(j-1) + " " + percentage)

        if (percentage > 0.25) {
          //println("Value " + (percentage-0.25))
          special = arr(j-1)
          found = true
        }

        count = 1


      }else {
        count = count+1
      }

    }

    if (found == false) {
      special = arr.last
    }

    special
  }
}