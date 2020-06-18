import scala.collection.mutable
import scala.util.control.Breaks._

object Solution {

  def isUgly(num: Int): Boolean = {
    if (num == 1) {
      true
    }else {
      if (num <= 0) {
        false
      }else {
        val uglySet = Set(2, 3, 5)
        var x = num
        breakable {
          while (true) {
            var divisible = false
            for (s <- uglySet if divisible == false) {
              if (x % s == 0) {
                x = x / s
                divisible = true
              }
            }

            if (divisible == false) {
              break
            }
          }
        }

        x == 1
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(isUgly(15))
  }
}