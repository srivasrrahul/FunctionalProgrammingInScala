import util.control.Breaks._
object Solution {
  def numberOfArithmeticSlices(arr: Array[Int]): Int = {

    def isArithmetic(start : Int,end : Int) : Option[Int] = {
      //println("start = " + start + " end " + end)
      val diff = arr(start+1) - arr(start)
      var ret_value = true
      breakable {
        for (j <- start + 1 to end) {
          if (arr(j) - arr(j-1) != diff) {
            ret_value = false
            break()
          }
        }
      }

      if (ret_value) {
        //println("yes")
        Some(diff)
      }else {
        //println("no")
        None
      }

    }

    if (arr.length < 3) {
      0
    }else {

      var count = 0
      for (j <- 0 to arr.length - 3) {
        var k = j+2
        val current_result = isArithmetic(j,k)
        current_result match {
          case Some(diff) => {
            count = count + 1
            breakable {
              for (i <- k+1 to arr.length-1) {
                if (arr(i) - arr(i-1) != diff) {
                  break()
                }else {
                  count = count + 1
                }
              }
            }
          }
          case None => {

          }
        }
      }

      count
    }
  }

  def main(args: Array[String]): Unit = {
    println(numberOfArithmeticSlices(Array(1,2,3,4)))

  }

}