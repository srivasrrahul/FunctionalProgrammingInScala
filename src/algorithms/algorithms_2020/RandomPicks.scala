import scala.collection.mutable
import util.control.Breaks._

class Solution(_w: Array[Int]) {

  val range_arr = Array.ofDim[Range](_w.length)
  var min_range = 0


  for (i <- 0 to _w.length-1) {


    range_arr(i) = min_range to (min_range+_w(i)-1)
    min_range =  range_arr(i).max+1


  }

  //println(range_arr.mkString(","))

  def bin_search_range(key : Int) : Int = {
    var low = 0
    var high = range_arr.length-1
    var res = -1
    //println("Searhcing for  " + key)
    breakable {
      while (low <= high && low >= 0 && high < range_arr.length) {
        //println(" " + low + " " + high)
        var mid = low + (high-low)/2
        //println("Mid is " + mid)
        //println(range_arr(mid).contains(key))
        if (range_arr(mid).contains(key)) {
          res = mid
          break()
        }else {
          if (range_arr(mid).max < key) {
            low = mid+1
          }else {
            high = mid-1
          }
        }
      }
    }

    if (res == -1) {
      //println("Problems ")
      res
    }else {
     res
    }

  }
  val r = new scala.util.Random

  def pickIndex(): Int = {
    val random_index = r.between(0,min_range)
    //println("Searching " + random_index)
    bin_search_range(random_index)
  }

}

object S1 {
  def main(args: Array[String]): Unit = {
    val s = new Solution(Array(10,5,5,1))
    //val s = new Solution(Array(1))
    var zero_count = 0
    var one_count = 0
    var two_count = 0
    var three_count = 0
    for (i <- 1 to 1000) {
      val index = s.pickIndex()
      index match {
        case 0 => zero_count = zero_count + 1
        case 1 => one_count = one_count + 1
        case 2 => two_count = two_count + 1
        case 3 => three_count = three_count + 1
      }
    }

    println(zero_count)
    println(one_count)
    println(two_count)
    println(three_count)


  }
}