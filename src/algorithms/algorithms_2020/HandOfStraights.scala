import scala.collection.mutable
import scala.util.control.Breaks._

object Solution {
  def isNStraightHand(hand: Array[Int], W: Int): Boolean = {
    if (hand.length % W != 0) {
      false
    }else {
      val sortedMultiMap = new mutable.TreeMap[Int, Int]()
      for (card <- hand) {
        val currentCount = sortedMultiMap.getOrElseUpdate(card, 0)
        sortedMultiMap += ((card, currentCount + 1))
      }

      //println(sortedMultiMap)
      var possible = true
      breakable {
        while (sortedMultiMap.size > 0) {
          //println(sortedMultiMap)
          var itr = sortedMultiMap.iterator
          var lastValueCollected: Option[Int] = None
          breakable {
            for (j <- 0 to W - 1) {
              if (itr.isEmpty == true) {
                possible = false
                break()
              }
              val shortestValues = itr.next()
              //println(shortestValues._1)
              if (shortestValues._2 > 1) {
                sortedMultiMap += ((shortestValues._1, shortestValues._2 - 1))
              } else {
                //Only one remove head and reset iterator
                sortedMultiMap.remove(shortestValues._1)
                itr = sortedMultiMap.iterator
              }

              lastValueCollected match {
                case None => lastValueCollected = Some(shortestValues._1)
                case Some(oldValue) => {
                  if (shortestValues._1 != oldValue + 1) {
                    //println(shortestValues._1 + " old " + oldValue)
                    possible = false
                    break
                  }else {
                    lastValueCollected = Some(shortestValues._1)
                  }
                }
              }
            }
          }

          if (possible == false) {
            break
          }
        }
      }

      possible
    }



  }

  def main(args: Array[String]): Unit = {
    println(isNStraightHand(Array(1,1,2,2,3,3),2))
  }
}