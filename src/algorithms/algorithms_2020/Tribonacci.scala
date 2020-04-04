object Solution {
  def tribonacci(n: Int): Int = {
    var t0 = 0
    var t1 = 1
    var t2 = 1

    n match {
      case 0 => 0
      case 1 => 1
      case 2 => 1
      case _ => {

        for (j <- 3 to n) {
          val t3 = t0 + t1 + t2
          t0 = t1
          t1 = t2
          t2 = t3
        }

        t2
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(tribonacci(25))
  }
}