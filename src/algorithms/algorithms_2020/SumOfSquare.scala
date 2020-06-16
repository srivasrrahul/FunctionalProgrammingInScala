import scala.collection.mutable

object Solution {
  def judgeSquareSum(c: Int): Boolean = {
    val sqrt = math.sqrt(c).toInt
    val set = new mutable.HashSet[Int]()
    var found = false
    for (j <- 0 to sqrt if found == false) {
      set.add(j*j)
      val pending = c - (j*j)
      if (set.contains(pending) == true) {
        found = true
      }
    }


//    for (p <- set if found == false) {
//      val pending = c - p
//      if (set.contains(pending) == true) {
//        found = true
//      }
//    }

    found
  }

  def main(args: Array[String]): Unit = {
    println(judgeSquareSum(3))
  }
}