import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    candidates.sortInPlace()

    def itr(candidateIndex : Int,pendingAmount : Int) : Option[List[List[Int]]] = {
      val Length = candidates.length - 1
      var usingCurrent: Option[List[List[Int]]] = None
      candidateIndex match {
        case Length if Length == candidates.length - 1 => {
          if (pendingAmount == candidates(candidateIndex)) {
            Some(List(List(candidates(candidateIndex))))
            //println("test")
          } else {
            None
          }
        }
        case _ => {
          val diff = pendingAmount - candidates(candidateIndex)

          val res = new mutable.HashSet[List[Int]]
          diff match {
            case 0 => {
              //Some(List(List(candidates(candidateIndex))))
              res.add(List(candidates(candidateIndex)))
            }
            case x if x > 0 => {
              usingCurrent = itr(candidateIndex + 1, diff)
              usingCurrent match {
                case None => {

                }
                case Some(usingCurrentLsts) => {
                  for (usingCurrentLst <- usingCurrentLsts) {
                    res.add(candidates(candidateIndex) :: usingCurrentLst)
                  }
                }
              }
            }
            case _ => {

            }
          }

          //Now ignore current
          itr(candidateIndex + 1, pendingAmount) match {
            case None => {

            }
            case Some(ignoreLsts) => {
              res.addAll(ignoreLsts)
            }
          }

          if (res.size > 0) {
            Some(res.toList)
          }else {
            None
          }
        }

      }
    }

    itr(0,target) match {
      case None => List()
      case Some(lsts) => lsts
    }

  }

  def main(args: Array[String]): Unit = {
    println(combinationSum2(Array(10,1,2,7,6,1,5),8))
    //1,2,2,5
    //println(combinationSum2(Array(2,5,2,1,2),5))
  }
}