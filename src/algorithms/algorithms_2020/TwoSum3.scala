import scala.collection.mutable
import scala.util.control.Breaks._

class TwoSum() {

  /** Initialize your data structure here. */

  val valCount = new mutable.TreeMap[Int,Int]

  /** Add the number to an internal data structure.. */
  def add(number: Int) : Unit = {
    valCount.get(number) match {
      case None => {
        valCount += ((number,1))
      }
      case Some(count) => {
        valCount += ((number,count+1))
      }
    }
  }

  /** Find if there exists any pair of numbers which sum is equal to the value. */
  def find(value: Int): Boolean = {
    var isSumFound = false
    breakable {
      for (m <- valCount) {
        val key = m._1
        val count = m._2

        val diff = value - key
        if (diff != key) {
          if (valCount.contains(diff)) {
            isSumFound = true
            break
          }
        }else {
          if (count > 1) {
            isSumFound = true
            break()
          }
        }
      }


    }

    isSumFound

  }



}

object Solution {
  def main(args: Array[String]): Unit = {
    val twoSum = new TwoSum
    twoSum.add(2)
    twoSum.add(1)
    println(twoSum.find(4))
  }
}