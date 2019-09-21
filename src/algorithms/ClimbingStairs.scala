import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object Solution {
//  def climbStairs(n: Int): Int = {
//    val cache = new HashMap[Int,List[List[Int]]]()
//    def itr(index : Int) : List[List[Int]] = {
//      //println(index)
//      index match {
//        case x if x > n => {
//          List()
//        }
//        case x if x == n => {
//          List(List(n))
//        }
//        case x if x == 0 => {
//          val res1 = itr(index+1)
//          val res2 = itr(index+2)
//
//          res1 ++ res2
//        }
//        case _ => {
//          cache.get(index) match {
//            case Some(l) => {
//              println("cache hit")
//              l
//            }
//            case None => {
//              val res1 = itr(index+1)
//              val res2 = itr(index+2)
//
//              val lstBufffer = new ListBuffer[List[Int]]
//
//              for (x <- res1) {
//                lstBufffer += (index :: x)
//              }
//
//              for (x <- res2) {
//                lstBufffer += (index :: x)
//              }
//
//              val l = lstBufffer.toList
//              cache += (index -> l)
//              lstBufffer.toList
//            }
//          }
//
//
//        }
//      }
//    }


//
//    val l = itr(0)
//    //println(l.mkString(","))
//    l.length
//  }


  def climbStairs(n: Int) : Int = {
    val arr = new Array[Long](n)
    arr(0) = 1
    if (n > 1) {
      arr(1) = 2
      for (j <- 2 to arr.length-1) {
        arr(j) = arr(j-1) + arr(j-2)
      }

      arr(arr.length-1).toInt
    }else {
      1
    }



  }



  def main(args: Array[String]): Unit = {
    println(climbStairs(89))
  }
}