import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    candidates.sortInPlace()

    //5 => 1,2
    def itr(pendingTarget : Int) : Option[Set[List[Int]]] = {
      val result = new mutable.HashSet[List[Int]]
      for (candidate <- candidates) {
        val diff = pendingTarget - candidate
        diff match {
          case 0 => {
            result.add(List(candidate))
          }
          case x if x > 0 => {
            itr(diff) match {
              case None => {

              }
              case Some(pendingLsts) => {
                for (pendingLst <- pendingLsts) {
                  result.add((candidate :: pendingLst).toSeq.sorted)
                }
              }
            }
          }
          case _ => {
            //less than zero nothing
          }
        }
      }

      if (result.size > 0) {
        Some(result.toSet)
      }else {
        None
      }
    }

    itr(target) match {
      case None => List()
      case Some(sets) => {
        sets.toList
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(combinationSum(Array(2,3,6,7),7))
  }
}