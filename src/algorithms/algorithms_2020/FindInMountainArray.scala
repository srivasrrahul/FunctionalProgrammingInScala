


 abstract class MountainArray {
     def get(index: Int): Int
     def length(): Int
 }


object Solution {
  def findInMountainArray(value: Int, mountainArr: MountainArray): Int = {
    //val len = A.length
    def peakIndexInMountainArray(A: MountainArray): Int = {
      if (A.length() == 3) {
        A.get(1)
      }else {
        def itr(begin : Int,end : Int) : Int = {
          if (end==begin) {
            begin
          }else {
            val mid = begin + (end-begin)/2
            if (A.get(mid) > A.get(mid+1)) {
              itr(begin,mid)
            }else {
              itr(mid+1,end)
            }
          }
        }

        itr(0,A.length()-1)
      }
    }

    if (mountainArr.length() == 3) {
     var foundIndex = -1
      for (j <- 0 to mountainArr.length()-1) {
        if (mountainArr.get(j) == value) {
          foundIndex = j
        }
      }

      foundIndex
    }else {
      val peakIndex = peakIndexInMountainArray(mountainArr)
      var sortedOrder = 0

      def binarySearch(begin: Int, end: Int): Int = {
        if (begin == end) {
          if (mountainArr.get(begin) == value) {
            begin
          } else {
            -1
          }
        } else {
          val mid = begin + (end - begin) / 2
          if (sortedOrder == 0) {
            //Ascending
            val midValue = mountainArr.get(mid)
            if (midValue == value) {
              mid
            } else {
              if (midValue < value) {
                binarySearch(mid + 1, end)
              } else {
                binarySearch(begin, mid)
              }
            }
          } else {
            val midValue = mountainArr.get(mid)
            if (midValue == value) {
              mid
            } else {
              if (midValue > value) {
                binarySearch(mid + 1, end)
              } else {
                binarySearch(begin, mid)
              }
            }
          }
        }
      }

      val peakVal = mountainArr.get(peakIndex)
      if (peakVal == value) {
        peakIndex
      } else {
        val r1 = binarySearch(0, peakIndex)
        if (r1 == -1) {
          sortedOrder = 1
          binarySearch(peakIndex + 1, mountainArr.length())
        } else {
          r1
        }

      }
    }

  }
}