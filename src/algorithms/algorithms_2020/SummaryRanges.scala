import scala.collection.mutable.ListBuffer

object Solution {
  def summaryRanges(nums: Array[Int]): List[String] = {

    if (nums.length == 0) {
      List()
    }else {
      if (nums.length == 1) {
        List(nums(0).toString)
      }else {
        val lstBuffer = new ListBuffer[String]
        var begin = nums(0)

        //1,2,3,4,5,8
        for (j <- 1 to nums.length-1) {
          if (nums(j) == nums(j-1)+1) {
            //Same as last
          }else {
            if (nums(j-1) == begin) {
              lstBuffer.append(nums(j-1).toString)
            }else {
              val string = begin.toString + "->" + nums(j - 1).toString
              lstBuffer.append(string)

            }
            begin = nums(j)
          }

        }

        //treat last

        if (nums.last == nums(nums.length-2) + 1) {
          val string = begin.toString + "->" + nums.last.toString
          lstBuffer.append(string)
        }else {
          lstBuffer.append(nums.last.toString)
        }

        lstBuffer.toList
      }
    }



  }

  def main(args: Array[String]): Unit = {
    println(summaryRanges(Array(0,2,3,4,10,11,12,14,17,18)))
  }
}