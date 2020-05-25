import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object Solution {
  def findNumberOfLIS(nums: Array[Int]): Int = {
    if (nums.length > 0) {
      val largestLIS = new Array[List[List[Int]]](nums.length)
      largestLIS(0) = List(List(nums(0)))
      var globalMax = 1
      for (j <- 1 to nums.length - 1) {
        val solution = new ListBuffer[List[Int]]
        var maxSizeTillNow = 0
        //println(nums(j))
        for (k <- j - 1 to 0 by -1) {
          //println(j + " " + k)
          val lsts = largestLIS(k)
          for (lst <- lsts) {
            if (lst.last < nums(j)) {
              //println(lst.size+1)
              if (lst.size + 1 >= maxSizeTillNow) {
                solution.append((nums(j) :: lst.reverse).reverse)
                maxSizeTillNow = lst.size + 1
              }
            }
          }

        }
        //start from here
        if (maxSizeTillNow < 1) {
          solution.append(List(nums(j)))
          maxSizeTillNow = 1
        }

        //filter which is not at least the size
        solution.filterInPlace(lst => {
          lst.size >= maxSizeTillNow
        })

        if (maxSizeTillNow > globalMax) {
          globalMax = maxSizeTillNow
        }


        largestLIS(j) = solution.toList
      }

      println(largestLIS.mkString("\n"))

      //println("global max " + globalMax)

      var count = 0
      largestLIS.foreach(lsts => {
        for (lst <- lsts) {
          println(lst)
          if (lst.size >= globalMax) {
            count = count + 1
          }
        }
      })

      count
    }else {
      0
    }
  }

  def main(args: Array[String]): Unit = {
    println(findNumberOfLIS(Array(1,3,5,4,7)))
  }

}