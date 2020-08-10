object Solution {
  def peakIndexInMountainArray(A: Array[Int]): Int = {
    if (A.length == 3) {
      A(1)
    }else {
      def itr(begin : Int,end : Int) : Int = {
        if (end==begin) {
          begin
        }else {
          val mid = begin + (end-begin)/2
          if (A(mid) > A(mid+1)) {
            itr(begin,mid)
          }else {
            itr(mid+1,end)
          }
        }
      }

      itr(0,A.length-1)
    }
  }

  def main(args: Array[String]): Unit = {

    println(peakIndexInMountainArray(Array(0,1,2,3,4,5,6,7,6,5,4,1)))
  }
}