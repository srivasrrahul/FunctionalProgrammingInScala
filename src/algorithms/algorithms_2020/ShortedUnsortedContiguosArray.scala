object Solution {
  def findUnsortedSubarray(nums: Array[Int]): Int = {
    def mergeSort(begin : Int,end : Int) : Option[(Int,Int)] = {
      if (begin == end) {
        None
      }else {
        val mid = begin + (end-begin)/2
        val leftIndexes = mergeSort(begin,mid)
        val rightIndexes = mergeSort(mid+1,end)

        //merging
        var currentIndex : Option[(Int,Int)] = None
        var itr1 = begin
        var itr2 = mid+1

        var arrBuffer = new scala.collection.mutable.ArrayBuffer[Int]()
        while (itr1 <= mid && itr2 <= end) {
          if (nums(itr1) <= nums(itr2)) {
            arrBuffer.append(nums(itr1))
            itr1 = itr1 + 1
          }else {
            //so from itr1 to mid is current
            arrBuffer.append(nums(itr2))
            currentIndex match {
              case None => {
                currentIndex = Some((itr1,itr2))
              }
              case Some((prevLeft,prevRight)) => {
                currentIndex = Some((prevLeft,itr2))
              }
            }
            itr2 = itr2+1
          }
        }

        while (itr1 <= mid) {
          arrBuffer.append(nums(itr1))
          itr1 = itr1 + 1
        }

        while (itr2 <= end) {
          arrBuffer.append(nums(itr2))
          itr2 = itr2 + 1
        }
        //println(begin + " " + end + " " + currentIndex)

        for (j <- 0 to arrBuffer.length-1) {
          nums(begin+j) = arrBuffer(j)
        }
        //leftmost and rigtmost
        (leftIndexes,currentIndex,rightIndexes) match {
          case (None,None,None) => None
          case (_,None,None) => leftIndexes
          case (None,_,None) => currentIndex
          case (None,None,_) => rightIndexes
          case (Some((ll,lr)),Some((ml,mr)),None) => {
            Some((math.min(ll,ml),math.max(lr,mr)))
          }
          case (Some((ll,lr)),None,Some((rl,rr))) => {
            Some((math.min(ll,rl),math.max(lr,rr)))
          }
          case (None,Some((ml,mr)),Some((rl,rr))) => {
            Some((math.min(ml,rl),math.max(mr,rr)))
          }
          case (Some((ll,lr)),Some((ml,mr)),Some((rl,rr))) => {
            val a1 = Array(ll,ml,rl)
            val a2 = Array(lr,mr,rr)
            Some((a1.min,a2.max))
          }
        }

      }
    }

    if (nums.length == 0) {
      0
    }else {
      mergeSort(0, nums.length - 1) match {
        case None => 0
        case Some((l, r)) => {
          //println(l)
          r - l + 1
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(findUnsortedSubarray(Array(2, 6, 4, 8, 10, 9, 15)))
  }
}