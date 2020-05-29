import scala.collection.Searching

object Solution {
  def threeSumClosest(nums: Array[Int], target: Int): Int = {
    nums.sortInPlace()
    //println(nums.mkString(","))
    var closeNess = Int.MaxValue
    var solution = Int.MaxValue
    for (j <- 0 to nums.length-3) {
      for (k <- j+1 to nums.length-2) {
        //println()
        val missing = target - (nums(j) + nums(k))
        if (k+1 == nums.length-1) {
          //only one option
          val diff = scala.math.abs(target - (nums(j) + nums(k) + nums(k+1)))
          if (diff < closeNess) {
            closeNess = diff
            solution = nums(j) + nums(k) + nums(k+1)
          }
        }else {
          nums.search(missing, k + 1, nums.length - 1) match {
            case Searching.Found(foundIndex) => {
              //println("Here")
              closeNess = 0
              solution = nums(j) + nums(k) + nums(foundIndex)
            }
            case Searching.InsertionPoint(possibleIndex) => {
              var whichIndex = possibleIndex
              var leftIndex = whichIndex-1
              var rightIndex = whichIndex+1

              if (leftIndex < k+1) {
                leftIndex = k + 1
              }

              if (rightIndex > nums.length-1) {
                rightIndex = nums.length-1
              }

              println(j + " " + k + " " + leftIndex + " " + rightIndex + " " + whichIndex)
              val opt1 = scala.math.abs(target - (nums(j) + nums(k) + nums(leftIndex)))
              val opt2 = scala.math.abs(target - (nums(j) + nums(k) + nums(rightIndex)))
              val opt3 = scala.math.abs(target - (nums(j) + nums(k) + nums(whichIndex)))

              if (opt1 < closeNess) {
                closeNess = opt1
                solution = nums(j) + nums(k) + nums(leftIndex)
              }

              if (opt3 < closeNess) {
                closeNess = opt3
                solution = nums(j) + nums(k) + nums(whichIndex)
              }




            }
          }
        }
      }
    }

    solution
  }

  def main(args: Array[String]): Unit = {
    println(threeSumClosest(Array(1,2,5,10,11),12))
    //-4,-1,1,2
  }
}