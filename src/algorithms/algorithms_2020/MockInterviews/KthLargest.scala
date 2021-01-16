object Solution {
  def findKthLargest(nums: Array[Int], k: Int): Int = {

    def swap(j : Int,k : Int) : Unit = {
      val temp = nums(j)
      nums(j) = nums(k)
      nums(k) = temp
    }

    def findPivot(begin : Int,end : Int) : Int = {

      val pivot = nums(begin)
      var swapperIndex = begin

      var j = end
      while (j > swapperIndex) {
        if (nums(j) >= pivot) {
          j = j - 1
        }else {
          swapperIndex = swapperIndex+1
          swap(swapperIndex,j)
        }
      }

      swap(begin,swapperIndex)
      swapperIndex
    }
    def itr(begin : Int,end : Int,index : Int) : Int = {
      //println(nums.mkString(",") + " " + begin + " " + end + " : " + index)
      if (begin == end) {
        nums(index)
      }else {
        val pivotIndex = findPivot(begin, end)
        //println(nums.mkString(",") + " " + begin + " " + end + " : " + index + " => " + pivotIndex)
        if (index == pivotIndex) {
          nums(pivotIndex)
        }else {
          if (index < pivotIndex) {
            itr(begin, pivotIndex, index)
          }else {
            itr(pivotIndex+1,end,index)
          }
        }
      }


    }

    val x = itr(0,nums.length-1,nums.length-k)
    //println(nums.mkString(","))
    x
  }

  def main(args: Array[String]): Unit = {
    println(findKthLargest(Array(10,20,11,13,14,7),5))
  }
}