import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
//  def integerBreak(n: Int): Int = {
//    val cache = new mutable.HashMap[Int,List[List[Int]]]()
//    def itr(j : Int) : List[List[Int]] = {
//      cache.get(j) match  {
//        case Some(lst) => {
//          //println("cahce hit")
//          lst
//        }
//        case None => {
//          j match {
//            case 2 => {
//              List(List(1, 1))
//            }
//            case _ => {
//              val retValue = new ListBuffer[List[Int]]
//              for (k <- j - 1 to 2 by -1) {
//                val retLst = itr(k)
//                for (elem <- retLst) {
//                  val updatedLst = (j - k) :: elem
//                  retValue.append(updatedLst)
//                }
//              }
//
//              for (x <- 1 to j - 1) {
//                retValue += List(x, j - x)
//              }
//              //retValue += List(1,j-1)
//              val lst = retValue.toList
//              cache += (j -> lst)
//              lst
//            }
//          }
//        }
//      }
//    }
//
//    val lst = itr(n)
//    //println(lst.mkString(","))
//    var maxProduct = 1
//    for (elem <- lst) {
//      val res = elem.foldRight(1)((x , y) => (x * y))
//      if (res > maxProduct) {
//        maxProduct = res
//      }
//    }
//
//    maxProduct
//  }


  def integerBreak(n : Int) : Int = {
    val arr = Array.ofDim[List[List[Int]]](n+1)
    arr(2) = List(List(1,1))
    for (j <- 3 to n) {
      val updatedVal = new ListBuffer[List[Int]]
      for (k <- 2 to j-1) {
        val lst = arr(k)
        for (elem <- lst) {
          updatedVal += ((j-k) :: elem)
        }

      }

      for (x <- 1 to j/2) {
        updatedVal += List(x,j-x)
      }

      arr(j) = updatedVal.toList
    }

    val finalLst = arr(n)
    var maxProduct = 1
    for (elem <- finalLst) {
      val res = elem.foldRight(1)((x , y) => (x * y))
      if (res > maxProduct) {
        maxProduct = res
      }
    }

    //println(arr.mkString(","))
    maxProduct
  }

  def main(args: Array[String]): Unit = {
    println(integerBreak(10))
//    println(integerBreak(3))
//    println(integerBreak(4))
  }
}