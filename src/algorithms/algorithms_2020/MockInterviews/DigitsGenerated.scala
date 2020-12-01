import scala.collection.mutable

object Solution {
  def atMostNGivenDigitSet(digits: Array[String], n: Int): Int = {
    val set = new mutable.HashSet[Int]()
    for (dig <- digits) {
      if (dig.toInt <= n) {
        set.add(dig.toInt)
      }
    }

    //println(set)

    var count = 0
    def walkDDigit(D : Int) : Unit = {
      def generate(earlier : Long,d : Int) : Unit = {
        if (d == D) {
          if (earlier <= n.toLong) {
            count = count+1
          }
        }else {
          for (dig <- set) {
            val newDigit = earlier*10+dig
            generate(newDigit,d+1)
          }
        }
      }

      for (dig <- set) {
        generate(dig,1)
      }
    }


    val len = n.toString.length
    for (j <- 1 to len) {
      walkDDigit(j)
    }

    count
  }

  def main(args: Array[String]): Unit = {
    println(atMostNGivenDigitSet(Array("1","3","5","7"),100))
  }
}