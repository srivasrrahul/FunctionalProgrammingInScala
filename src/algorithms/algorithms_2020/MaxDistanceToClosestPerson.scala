import scala.collection.mutable

object Solution {
  def maxDistToClosest(seats: Array[Int]): Int = {

    val leftOccupiedSeats = new Array[Int](seats.length)

    var leftEmptyLen = 0
    for (j <- 0 to leftOccupiedSeats.length-1) {
      if (seats(j) == 0) {
        //its unoccupied
        leftEmptyLen = leftEmptyLen + 1
        leftOccupiedSeats(j) = leftEmptyLen

      }else {
        leftEmptyLen = 0
      }
    }

    val rightOccupiedSeats = new Array[Int](seats.length)
    var rightEmptyLen = 0
    for (j <- rightOccupiedSeats.length-1 to 0 by -1) {
      if (seats(j) == 0) {
        //its unoccupied
        rightEmptyLen = rightEmptyLen + 1
        rightOccupiedSeats(j) = rightEmptyLen
      }else {
        rightEmptyLen = 0
      }
    }

//    println(leftOccupiedSeats.mkString(","))
//    println(rightOccupiedSeats.mkString(","))
    //0,0,0,0
    var maxDist = 0
    val N = seats.length-1

    for (j <- 0 to seats.length-1) {

      if (seats(j) == 0) {
        j match {
          case 0 => {
            val localMaxDist = rightOccupiedSeats(j)
            if (localMaxDist > maxDist) {
              maxDist = localMaxDist
            }
          }
          case N => {
            val localMaxDist = leftOccupiedSeats(j)
            if (localMaxDist > maxDist) {
              maxDist = localMaxDist
            }
          }
          case _ => {
            val localMaxDist = scala.math.min(leftOccupiedSeats(j),rightOccupiedSeats(j))
            if (localMaxDist > maxDist) {
              maxDist = localMaxDist
            }
          }
        }

      }
    }

    maxDist


  }

  def main(args: Array[String]): Unit = {
    println(maxDistToClosest(Array(1,0,0,0,1,0,1)))
  }
}