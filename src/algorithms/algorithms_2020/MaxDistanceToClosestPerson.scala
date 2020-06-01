import scala.collection.mutable

object Solution {
  def maxDistToClosest(seats: Array[Int]): Int = {
    val occupiedSeats = new mutable.TreeSet[Int]()
    val unoccupiedSeats = new mutable.HashSet[Int]()
    for (j <- 0 to seats.length-1) {
      if (seats(j) == 1) {
        occupiedSeats.add(j)
      }else {
        unoccupiedSeats.add(j)
      }
    }

    var maxDist : Option[Int] = None
    for (unoccupiedSeatIndex <- unoccupiedSeats) {
      //println("US " + unoccupiedSeatIndex)
      val leftTree = occupiedSeats.rangeUntil(unoccupiedSeatIndex)
      val righTree = occupiedSeats.rangeFrom(unoccupiedSeatIndex)

      //println(leftTree)
      //println(righTree)

      var localMaxDist = Int.MaxValue
      if (unoccupiedSeatIndex != 0) {
        if (leftTree.isEmpty == false) {
          val leftDistance = unoccupiedSeatIndex - leftTree.last
          localMaxDist = leftDistance
        }
      }

      if (unoccupiedSeatIndex != seats.length-1) {
        if (righTree.isEmpty == false) {
          val rightDistance = righTree.head - unoccupiedSeatIndex
          if (rightDistance < localMaxDist) {
            localMaxDist = rightDistance
          }
        }
      }

      maxDist match {
        case None => maxDist = Some(localMaxDist)
        case Some(earlierMaxDist) => {
          if (localMaxDist > earlierMaxDist) {
            maxDist = Some(localMaxDist)
          }
        }
      }
    }

    maxDist.get


  }

  def main(args: Array[String]): Unit = {
    println(maxDistToClosest(Array(1,0,0,0)))
  }
}