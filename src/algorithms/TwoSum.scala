import scala.collection.Searching._
import util.control.Breaks._


object Solution {

  def twoSum(num : Array[Int],target : Int) : Array[Int] = {
    val nums = num.sortWith(_ < _)
    //println(num.mkString(","))
    val response = new Array[Int](2)
    //println(nums.mkString(","))
    breakable {
      for (i <- 0 to nums.length - 2) {
        val pending = target - nums(i)
        //println(pending + " => " + (i+1) + "," + (nums.length-1))
        val res = nums.search(pending, i + 1, nums.length)
        res match {
          case Found(j) => {
            response(0) = i
            response(1) = j
            //println("Found answer")
            break
          }
          case InsertionPoint(_) => {

          }

        }
      }
    }
    //println(response.mkString(","))
    val updatedResponse = new Array[Int](2)
    updatedResponse(0) = -1
    updatedResponse(0) = -1
    val v1 = nums(response(0))
    val v2 = nums(response(1))

    for (i <- 0 to num.length-1) {
      //println(" i = " + i + " " + num(i) + " " + v1 + ", " + v2)
      if (num(i) == v1 && updatedResponse(0) == -1) {
        updatedResponse(0) = i
      }
      else {
        if (num(i) == v2) {
          updatedResponse(1) = i
        }
      }
    }

    updatedResponse.sortWith(_ < _)

  }



//  def main(args: Array[String]): Unit = {
//    println(twoSum(Array(2,5,5,11),10).mkString(","))
//  }
}



