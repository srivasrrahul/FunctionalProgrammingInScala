import scala.util.control.Breaks._
object TupleOrdering extends Ordering[(Int,Int)] {
  override def compare(x: (Int, Int), y: (Int, Int)): Int = {
    val len1 = x._2 - x._1 + 1
    val len2 = y._2 - y._1 + 1

    len1.compareTo(len2)

  }
}
object Solution {
  def characterReplacement(s: String, k: Int): Int = {
    //for first its only 1 so lets start with second index
    if (s.length == 0) {
      0
    } else {
      val sizes = new Array[(Int, Int)](s.length)

      sizes(0) = (0, 0)
      for (j <- 1 to s.length - 1) {
        //Start from here and make everybody equal to me
        var pendingCount = k
        var maxIndex = j
        breakable {
          for (k <- j - 1 to 0 by -1) {
            if (s.charAt(k) == s.charAt(j)) {
              maxIndex = k
            } else {
              if (pendingCount > 0) {
                //flip
                pendingCount = pendingCount - 1
                maxIndex = k
              } else {
                break
              }
            }
          }
        }

        sizes(j) = (maxIndex, j)

        if (k >= 1) {
          pendingCount = k - 1 //flip current to prev
          maxIndex = j - 1
          breakable {
            for (k <- j - 2 to 0 by -1) {
              if (s.charAt(k) == s.charAt(j - 1)) {
                maxIndex = k
              } else {
                if (pendingCount > 0) {
                  //flip
                  pendingCount = pendingCount - 1
                  maxIndex = k
                } else {
                  break
                }
              }
            }
          }

          val obtainedLen = j - maxIndex + 1
          val currentSize = sizes(j)._2 - sizes(j)._1 + 1
          if (obtainedLen > currentSize) {
            sizes(j) = (maxIndex, j)
          }
        }
      }

      //println(sizes.mkString(" , "))
      val maxTuple = sizes.max(TupleOrdering)
      maxTuple._2 - maxTuple._1 + 1
    }
  }

  def main(args: Array[String]): Unit = {
    //println(characterReplacement("BAAAB",2))
    println(characterReplacement("ABAB",2))
  }
}