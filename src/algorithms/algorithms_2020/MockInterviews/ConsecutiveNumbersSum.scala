import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

case class Index(val value : Int,val beginsWith : Int)
object Solution {
  def consecutiveNumbersSum(N: Int): Int = {
    //val cache = new mutable.HashMap[Index,Int]()
    def itr(value : Int,beginsWith : Int) : List[List[Int]] = {
      if (value < beginsWith) {
        List() //empty list
      }else {
        if (value == beginsWith) {
          List(List(value))
        } else {
          val pendingLsts = itr(value-beginsWith, beginsWith+1)
          if (pendingLsts.isEmpty == false) {
            val lstBuffer = new ListBuffer[List[Int]]
            for (pendingLst <- pendingLsts) {
              lstBuffer.append(beginsWith :: pendingLst)
            }

            lstBuffer.toList
          } else {
            List()
          }
        }
      }
    }

    var count = 0
    for (j <- 1 to N/2) {
      val lsts = itr(N-j,j+1)
      //println(j + " " + lsts)
      if (lsts.isEmpty == false) {
        count = count+1
      }
    }

    count+1 //count self
  }

  def main(args: Array[String]): Unit = {
    println(consecutiveNumbersSum(15))
  }


}