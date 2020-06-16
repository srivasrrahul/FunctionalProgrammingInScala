import scala.collection.mutable

object Solution {
  def numSquares(n: Int): Int = {
    val sqrt = scala.math.sqrt(n).toInt

    val set = new mutable.TreeSet[Int]()
    for (j <- 1 to sqrt) {
      set.add(j*j)
    }

    val map = new mutable.HashMap[Int,Option[Int]]()
    for (j <- 1 to sqrt) {
      map += (((j*j),Some(1)))
    }


    for (j <- 1 to n) {
      map.get(j) match {
        case None => {
          val range = set.rangeUntil(j)
          var minCount : Option[Int] = None

          for (k <- range) {
            val diff = j - k
            map.get(diff) match {
              case None => {}
              case Some(count) => {
                count match {
                  case None => {}
                  case Some(valCount) => {
                    val newCount = 1 + valCount
                    minCount match {
                      case None => {
                        minCount = Some(newCount)
                      }
                      case Some(oldCount) => {
                        if (newCount < oldCount) {
                          minCount = Some(newCount)
                        }
                      }
                    }
                  }
                }
              }
            }
          }

          map += ((j,minCount))
        }
        case Some(lst) => {

        }
      }
    }

    //println(set)
    //println(map)
    map.get(n).get.get


  }

  def main(args: Array[String]): Unit = {
    println(numSquares(13))
  }
}