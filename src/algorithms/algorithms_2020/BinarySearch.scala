import util.control.Breaks._
object Solution {
  def binary_search(arr :Array[Int],key : Int) : Int = {
    var low = 0
    var high = arr.length-1
    var index = -1
    breakable {
      while (high >= low) {
        if (low == high) {
          if (arr(low) != key) {
            index = -1
            break
          }else {
            index = low
            break
          }
        }else {
          val mid = low + (high-low)/2
          if (arr(mid) < key) {
            low = mid+1
          }else {
            if (arr(mid) > key) {
              high = mid-1
            }else {
              index = mid
              break()
            }
          }
        }
      }
    }

    index
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1,2,3,10,20,30)
    println(binary_search(arr,-1))
  }
}