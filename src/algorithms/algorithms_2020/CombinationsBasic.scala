import scala.collection.mutable.ListBuffer

object Solution {
  def combine(N: Int, k: Int): List[List[Int]] = {
    //4,2  => (1,2)
    //(1,2) => (1 = 4) => No,diff = 4-1 = 3, itr(2,1)
    //itr(2,1) => (2==4) => No,diff = 1-1 = 0,Some(2), itr(3,1)
    def itr(currentIndex : Int,pendingK : Int) : Option[List[List[Int]]] = {
      currentIndex match {
        case N => {
          if (pendingK == 1) {
            Some(List(List(currentIndex)))
          }else {
            None
          }
        }
        case _ => {
          val res = new ListBuffer[List[Int]]
          val diff = pendingK - 1
          diff match {
            case 0 => {
              res.append(List(currentIndex))
            }
            case x if x > 0 => {
              itr(currentIndex+1,diff) match {
                case None => {

                }
                case Some(usedCurrentLsts) => {
                  for (usedCurrentLst <- usedCurrentLsts) {
                    res.append(currentIndex :: usedCurrentLst)
                  }
                }
              }
            }
            case _ => {
              //ignore
            }
          }

          //ignore current
          itr(currentIndex+1,pendingK) match {
            case None => {

            }
            case Some(ignoredLsts) => {
              res.addAll(ignoredLsts)
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

    itr(1,k) match {
      case None => {
        List()
      }
      case Some(lsts) => {
        lsts
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(combine(4,2))
  }
}