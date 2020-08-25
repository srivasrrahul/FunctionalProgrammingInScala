import scala.collection.mutable
case class Index(val x : Int,val y : Int)
object Solution {
  def numTrees(n: Int): Int = {
    val matrix = Array.ofDim[Int](n+1,n+1)
    for (j<-1 to matrix.length-1) {
      matrix(j)(j) = 1
    }

    var j = 1
    var current = 2
    var k = current

    while (j < matrix.length && k < matrix(0).length) {
      var total = 0
      for (r <- j to k) {

        var leftSubTreeCount = 0
        if (r > j) {
          leftSubTreeCount = matrix(j)(r-1)
        }

        var rightSubTreeCount = 0
        if (r+1 <= k) {
          rightSubTreeCount = matrix(r+1)(k)
        }

        var currentTotal = 0
        if (leftSubTreeCount == 0 || rightSubTreeCount == 0) {
          currentTotal = math.max(leftSubTreeCount,rightSubTreeCount)
        }else {
          currentTotal = leftSubTreeCount * rightSubTreeCount
        }

        total = total + currentTotal

      }

      matrix(j)(k) = total

      j = j + 1
      k = k + 1

      if (k >= matrix(0).length) {
        current = current+1
        j = 1
        k = current
      }
    }

    matrix(1).last
  }

  def main(args: Array[String]): Unit = {
    println(numTrees(3))
  }
}