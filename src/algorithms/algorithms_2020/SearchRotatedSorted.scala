object Solution {
  def search(nums: Array[Int], target: Int): Int = {
    nums.isEmpty match {
      case true => -1
      case _ => {
        def findPivot(begin: Int, end: Int): Int = {
          //println("begin end " + begin + "," + end)
          if (begin > end) {
            -1
          } else {
            if (begin == end) {
              begin
            } else {
              val mid = begin + (end - begin) / 2
              val midVal = nums(mid)

              val leftVal = nums(begin)
              val rightVal = nums(end)

              val nextIndex = if (mid+1 > end) end else (mid+1)
              val priorIndex = if (mid-1 < begin) begin else (mid-1)
              //if (midVal > nums(mid + 1) && midVal > nums(mid - 1)) {
              if (midVal >= nums(nextIndex) && midVal >= nums(priorIndex)) {
                mid
              } else {
                val midGreaterThanLeft = midVal >= leftVal
                val midGreaterThanRight = midVal >= rightVal

                (midGreaterThanLeft, midGreaterThanRight) match {
                  case (true, true) => {
                    findPivot(mid + 1, end)
                  }
                  case (true, false) => {
                    findPivot(mid + 1, end)
                  }
                  case (false, true) => {
                    findPivot(begin, mid - 1)
                  }
                  case (false, false) => {
                    //cant think of a case
                    findPivot(begin, mid - 1)
                  }
                }
              }
            }
          }
        }


        val pivotIndex = findPivot(0,nums.length-1)
        //println(pivotIndex)
        val pivotVal = nums(pivotIndex)

        val rightVal = nums.last
        val firstVal = nums.head

        val numsSeq = nums.toSeq

        //println("RIgjt " + (pivotIndex+1) + " end " + (numsSeq.length-1))
        numsSeq.search(target,pivotIndex+1,numsSeq.length) match {
          case scala.collection.Searching.Found(foundIndex) => {
            foundIndex
          }
          case r => {
            //println(r)
            numsSeq.search(target,0,pivotIndex+1) match {
              case scala.collection.Searching.Found(foundIndex1) => {
                foundIndex1
              }
              case _ => {
                -1
              }
            }
          }
        }
      }
    }



  }

  def main(args: Array[String]): Unit = {
    val arr = Array[Int](3,1)
    println(search(arr,1))

  }
}