import scala.collection.Searching
import scala.util.control.Breaks._

object Solution {
  def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
    var solution : Option[(Int,Int)] = None
    for (j <- 0 to numbers.length-2) {
      val diff = target - numbers(j)
      breakable {
        numbers.toSeq.search(diff,j+1,numbers.length) match {
          case Searching.Found(index) => {
            solution = Some(j+1,index+1)
            break
          }
          case _ => {

          }
        }
      }
    }

    val retValue = Array(solution.get._1,solution.get._2)
    retValue
  }
}