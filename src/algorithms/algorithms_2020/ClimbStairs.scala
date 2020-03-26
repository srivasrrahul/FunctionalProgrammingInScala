import scala.collection.mutable.ListBuffer

object Solution {
  def climbStairs(n: Int): Int = {

    if (n == 1) {
      1
    }else {
      if (n == 2) {
        2
      }else {
        val solution  = new Array[Int](2)
        solution(0) = 1
        solution(1) = 2

        for (j <- 2 to n-1) {
          val newSolution = solution(0) + solution(1)
          solution(0) = solution(1)
          solution(1) = newSolution


        }

        solution(1)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    println(climbStairs(4))
  }
}