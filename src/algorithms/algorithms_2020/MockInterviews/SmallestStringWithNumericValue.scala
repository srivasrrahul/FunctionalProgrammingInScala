import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val x : Int,val y : Int)
object Solution {
  def getSmallestString(N: Int, K: Int): String = {

    val stringBuilder = new StringBuilder
    for (j <- 0 to N-1) {
      stringBuilder.append('a')
    }

    var j = N-1
    var sum = N

    while (j >= 0 && sum < K) {
      if (sum  == K) {

      }else {
        val value = stringBuilder(j)
        if (value == 'z') {
          j = j-1
        }else {
          stringBuilder(j) = (value.toInt + 1).toChar
          sum = sum + 1
        }
      }
    }

    stringBuilder.toString()

//    val cache = new mutable.HashMap[Index,Option[String]]()
//    def itr(a : Int, b : Int) : Option[String] = {
//      if (a == 1) {
//        if (b >= 1 && b <= 26) {
//          Some(('a' + (b - 1)).toChar.toString)
//        } else {
//          None
//        }
//      } else {
//        val index = new Index(a,b)
//        if (cache.contains(index)) {
//          cache.get(index).get
//        }else {
//          val lst = new ListBuffer[String]
//          var breakLoop = false
//          for (j <- 1 to 26 if breakLoop == false) {
//            val next = itr(a - 1, b - j)
//            if (next.isDefined) {
//              lst.append(('a' + (j - 1)).toChar.toString + next.get)
//              breakLoop = true
//            }
//          }
//
//          //println(lst)
//          var retValue : Option[String] = None
//          if (lst.size > 0) {
//            retValue = Some(lst.min(new Ordering[String] {
//              override def compare(x: String, y: String): Int = {
//                x.compare(y)
//              }
//            }))
//          } else {
//            retValue = None
//          }
//
//          cache += ((index,retValue))
//          retValue
//        }
//
//      }
//    }
//
//    itr(n,k).get
  }

  def main(args: Array[String]): Unit = {
    println(getSmallestString(5,73))
  }
}