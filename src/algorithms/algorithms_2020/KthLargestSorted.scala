object Solution {
  def findKthLargest(nums: Array[Int], k: Int): Int = {
    def xchange(i: Int, j: Int): Unit = {
      val t = nums(i)
      nums(i) = nums(j)
      nums(j) = t
    }

    def pivot(beginIndex: Int, endIndex: Int): Int = {
      if (beginIndex == endIndex) {
        beginIndex //offset  can ever not be 0
      } else {
        val pivot = nums(endIndex)
        var i = beginIndex-1
        for (j <- beginIndex to endIndex - 1) {
          if (nums(j) <= pivot) {
            i = i + 1
            xchange(i, j)
          }
        }

        xchange(i + 1, endIndex)
        i + 1
      }
    }

    def explore(beginIndex: Int, endIndex: Int, offset: Int): Int = {
      //println("begin " + beginIndex + " endIndex " + endIndex + " offset " + offset)
      if (beginIndex >= endIndex) {
        nums(beginIndex) //offset can be ignored
      } else {
        val pivotIndex = pivot(beginIndex, endIndex)
        //println("Pivot is " + pivotIndex)
        if (pivotIndex == offset) {
          nums(pivotIndex)
        } else {
          if (pivotIndex > offset) {
            explore(beginIndex, pivotIndex - 1, offset)
          } else {
            explore(pivotIndex + 1, endIndex, pivotIndex + (offset - pivotIndex))
          }
        }
      }
    }

    explore(0,nums.length-1,nums.length-k)
  }




  def main(args: Array[String]): Unit = {
    println(findKthLargest(Array(3,2,3,1,2,4,5,5,6),4))
  }
}