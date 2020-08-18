import scala.collection.mutable

object Solution {
  def minDays(n: Int): Int = {
    val arr = new Array[Int](n+1)
    arr(0) = 0
    arr(1) = 1
    for (j <- 2 to n) {
      arr(j) = Array(1+arr(j-1),if (j%2 == 0) (1+arr(j/2)) else Int.MaxValue,if (j%3 == 0) (1+arr(j/3)) else Int.MaxValue).min
    }
    arr(n)
  }

  def main(args: Array[String]): Unit = {
    println(minDays(61455274))
  }
}