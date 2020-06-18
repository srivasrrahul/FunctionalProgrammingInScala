import scala.collection.mutable

object Solution {
  def isUgly(num: Int): Boolean = {
    if (num == 1) {
      true
    }else {
      if (num <= 0) {
        false
      }else {
        val bitSet = new mutable.BitSet()
        for (j <- 2 to num) {
          bitSet.add(j)
        }

        val sqrt = math.sqrt(num).toInt

        for (i <- 2 to sqrt) {
          if (bitSet.contains(i)) {
            var sq = i * i
            var k = 0
            while ((sq + k * i) <= num) {
              bitSet.remove(sq + k * i)
              k = k + 1
            }
          }
        }

        //println(bitSet)
        val uglySet = Set(2, 3, 5)
        var ugly = true
        val primeDivisors = new mutable.HashSet[Int]()
        for (prime <- bitSet) {
          if (num % prime == 0) {
            //prime is a divisor
            if (uglySet.contains(prime) == false) {
              ugly = false
            }
          }
        }

        ugly
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(isUgly(1641249143))
  }
}