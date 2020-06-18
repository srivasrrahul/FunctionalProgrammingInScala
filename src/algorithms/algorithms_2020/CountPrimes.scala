import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def countPrimes(n: Int): Int = {
    def checkPrime(x : Int) : Boolean = {
      val sqrt = scala.math.sqrt(x).toInt
      var isPrime = true
      for (y <- 2 to sqrt by 2 if isPrime == true) {
          if (y % x == 0) {
            isPrime = false
          }
      }

      isPrime
    }


    n match {
      case 0 => 0
      case 1 => 0
      case 2 => 0
      case 3 => 1
      case 4 => 2
      case 5 => 2
      case 6 => 3
      case 7 => 3
      case 8 => 4
      case 9 => 4
      case 10 => 4
      case _ => {
        val numberSet = new mutable.BitSet()
        for (j <- 2 to n-1) {
          numberSet.add(j)
        }

        val maxLimit = scala.math.sqrt(n-1).toInt

        for (i <- 2 to maxLimit) {
          if (numberSet.contains(i)) {
            var sq = i*i
            var j = sq
            var k = 0
            while (j < n) {
              numberSet.remove(j)
              k = k + 1
              j = sq + k*i
            }
          }
        }


        numberSet.size



      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(countPrimes(49979))
  }
}