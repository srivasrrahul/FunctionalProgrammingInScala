import scala.collection.mutable

object Solution {
  def twoSumLessThanK(A: Array[Int], K: Int): Int = {
    A.sortInPlace()
    var i = 0
    var j = A.length-1

    var solution = -1

    while (i < j) {
      if (A(i) + A(j) >= K) {
        j = j -1
      }else {
        solution = scala.math.max(solution,A(i) + A(j))
        i = i + 1
      }
    }

    solution
  }

  def main(args: Array[String]): Unit = {
    println(twoSumLessThanK(Array(34,23,1,24,75,33,54,8),60))
  }
}