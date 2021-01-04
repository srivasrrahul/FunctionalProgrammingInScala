import scala.collection.mutable.ListBuffer

object Solution {
  def countArrangement(n: Int): Int = {
    var count = 0
    def itr(j : Int,s : Set[Int]) : Unit = {
      if (j == n+1) {
        count = count+1
      }else {
        for (p <- s) {
          if (p % j == 0 || j % p == 0) {
            itr(j+1,s.-(p))
          }
        }
      }
    }

    itr(1,Range(1,n+1).toSet)
    count

  }

  def main(args: Array[String]): Unit = {
    println(countArrangement(4))
  }
}