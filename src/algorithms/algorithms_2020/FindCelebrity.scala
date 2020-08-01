import scala.collection.mutable
import scala.util.control.Breaks._
/* The knows API is defined in the parent class Relation.
      def knows(a: Int, b: Int): Boolean = {} */

trait Relation {
  def knows(a: Int, b: Int): Boolean = {
    false
  }
}
class Solution extends Relation {
  def findCelebrity(n: Int): Int = {
    val possibleCelebreties = new mutable.HashSet[Int]()
    for (j <- 0 to n-1) {
      possibleCelebreties.add(j)
    }

    //println(possibleCelebreties + " " +n)
    for (j <- 0 to n-1) {
      for (k <- 0 to n - 1) {
        if (j != k) {
          if (knows(k, j)) {
            //k knows j so k can't be
            possibleCelebreties.remove(k)
          } else {
            //k doesn't know j so j can't be
            possibleCelebreties.remove(j)
            //break
          }
        }
      }
    }

    //println(possibleCelebreties)
    if (possibleCelebreties.size == 1) {
      possibleCelebreties.head
    }else {
      -1
    }
  }

}