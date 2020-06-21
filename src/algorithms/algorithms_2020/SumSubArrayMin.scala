import scala.collection.mutable
import scala.util.control.Breaks._
object Solution {
  def sumSubarrayMins(A: Array[Int]): Int = {
    val pq = mutable.PriorityQueue.empty[(Int,Int)](Ordering.by((_:(Int,Int))._1).reverse)

    for (j <- 0 to A.length-1) {
      pq.addOne((A(j),j))
    }



    //println(pq)

    var lastIndex = -1
    var sum = 0
    val modValue = scala.math.pow(10,9).toInt + 7
    val analyzedIndexes = new mutable.HashSet[Int]()
    while (pq.isEmpty == false) {

      val top = pq.dequeue()
      //println(top)
      val currentIndex = top._2
      val topValue = top._1
      breakable {
        for (j <- top._2 to A.length - 1) {
          if (analyzedIndexes.contains(j) == true) {
            break()
          }

          breakable {
            for (k <- top._2 to 0 by -1) {
              if (analyzedIndexes.contains(k) == true) {
                break
              }
              //println(j + " " + k + " " + top._1)
              sum = (sum + top._1) % modValue
            }
          }
          //move to right

        }
      }

      analyzedIndexes.add(top._2)

    }

    sum


  }

  def main(args: Array[String]): Unit = {
    println(sumSubarrayMins(Array(3,1,2,4)))
  }
}