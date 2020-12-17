import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def fourSumCount(A: Array[Int], B: Array[Int], C: Array[Int], D: Array[Int]): Int = {
    val abSum = new mutable.ArrayBuffer[Int]()
    for (a <- A) {
      for (b <- B) {
        abSum.addOne(a+b)
      }
    }

    val cdSum = new mutable.HashMap[Int,Int]()
    for (c <- C) {
      for (d <- D) {
        val s = c+d
        val count = cdSum.getOrElseUpdate(s,0)
        cdSum += ((s,count+1))
      }
    }

    var retValue = 0
    for (ab <- abSum) {
      val pending = -ab
      retValue = retValue + cdSum.getOrElseUpdate(pending,0)
    }

    retValue
  }
}