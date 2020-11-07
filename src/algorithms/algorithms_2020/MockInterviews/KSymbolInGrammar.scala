import scala.collection.mutable.ListBuffer

object Solution {
  def kthGrammar(N: Int, K: Int): Int = {
    var lstBuffer = new ListBuffer[Int]
    var n = N
    var k = K-1
    while (n > 0) {
      lstBuffer.append(k)
      n = n-1
      k = k/2
    }

    //println(lst)
    //lst.append(k)
    val lst = lstBuffer.reverse.toList

    //println(lst)

    var prevVal = 0
    var prevIndex = lst.head
    var currentVal = 0
    for (currentIndex <- lst.tail) {
      if (currentIndex == prevIndex*2) {
        currentVal = prevVal
      }else {
        assert(currentIndex == (prevIndex*2+1))
        if (prevVal == 0) {
          currentVal = 1
        }else {
          currentVal = 0
        }
      }

      prevIndex = currentIndex
      prevVal = currentVal
    }

    currentVal
  }

  def main(args: Array[String]): Unit = {
    println(kthGrammar(4,5))
  }
}