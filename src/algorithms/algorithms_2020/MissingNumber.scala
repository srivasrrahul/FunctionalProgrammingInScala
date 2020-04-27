import scala.util.control.Breaks._
object Solution {
  def missingElement(nums: Array[Int], k: Int): Int = {

    def findMissing(begin : Int,end : Int,index : Int) : Int  = {
      var first = begin
      var last = end

      val range = Range(begin,end+1)
      println(range)
      range(index-1)
    }


    var missingPending = k
    var found = -1
    breakable {
      for (j <- 1 to nums.length - 1) {
        val diff = nums(j) - nums(j - 1) - 1
        if (diff >= missingPending) {
          //Number here

          //println("Missing pending " + (nums(j-1)+1) + " " + (nums(j)-1) + " : " + missingPending)
          found = findMissing(nums(j-1)+1,nums(j)-1,missingPending)
          missingPending = 0

          break
        } else {
          missingPending = missingPending - diff
        }

      }
    }

    if (missingPending > 0) {
      //still pending
      //it's uptill infinity
      found = Range(nums.last+1,Int.MaxValue)(missingPending-1)
    }

    found
  }

  def main(args: Array[String]): Unit = {
    println(missingElement(Array(4,7,9,10),6))
  }
}