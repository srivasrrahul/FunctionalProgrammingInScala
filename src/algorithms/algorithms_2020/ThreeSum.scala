import scala.collection.Searching.{Found, InsertionPoint}
import scala.collection.{Searching, mutable}
import scala.collection.mutable.ListBuffer


object Solution {

  def threeSum(nums: Array[Int]): List[List[Int]] = {
    val numsSorted = nums.sortWith(_ < _)
    val retValue = new mutable.HashSet[List[Int]]

    for (j <- 0 to numsSorted.length-1) {
      for (k <- j+1 to numsSorted.length-1) {
        val s = -(numsSorted(j) + numsSorted(k))
        numsSorted.search(s,k+1,numsSorted.length) match {
          case Searching.Found(f) => {
            val lst = List(numsSorted(j),numsSorted(k),s)
            retValue.addOne(lst)
          }
          case Searching.InsertionPoint(_) => {

          }
        }

      }
    }

    retValue.toList
  }



  def main(args: Array[String]): Unit = {
    println(threeSum(Array(-1, 0, 1, 2, -1, -4)))
  }
}