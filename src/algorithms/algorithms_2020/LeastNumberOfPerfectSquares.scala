import scala.collection.mutable

object Solution {
  def numSquares(n: Int): Int = {
    val sqrt = scala.math.sqrt(n).toInt

    val set = new mutable.TreeSet[Int]()
    for (j <- 1 to sqrt) {
      set.add(j*j)
    }

    //println(set)
    val memoizeMap = new mutable.HashMap[Int,Option[Int]]()
    def itr(current : Int) : Option[Int] = {
      //println(current)
      val smallest = set.head
      if (current < smallest) {
        None
      } else {
        if (set.contains(current)) {
          Some(1)
        } else {
          if (memoizeMap.contains(current)) {
            memoizeMap.get(current).get
          } else {
            val setRange = set.rangeUntil(current)
            var retValue: Option[Int] = None
            for (r <- setRange) {
              val diff = current - r
              val pending = itr(diff)
              pending match {
                case Some(value) => {
                  val count = 1 + value
                  retValue match {
                    case None => {
                      retValue = Some(count)
                    }
                    case Some(oldCount) => {
                      if (count < oldCount) {
                        retValue = Some(count)
                      }
                    }
                  }
                }
                case None => {}
              }
            }

            memoizeMap += ((current, retValue))
            retValue
          }
        }
      }
    }

    itr(n).get

  }

  def main(args: Array[String]): Unit = {
    println(numSquares(13))
  }
}