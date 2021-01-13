import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Solution {
  def numRescueBoats(people: Array[Int], limit: Int): Int = {
    people.sortInPlace()
    var j = 0
    var k = people.length-1


    var count = 0
    while (j <= k) {
      //println(j + " " + k + " : " + count)
      if (j == k) {
        count = count+1
        j = j +1
        k = k -1
      }else {
        val t = people(j) + people(k)
        if (t <= limit) {
          count = count+1
          j = j + 1
          k = k - 1
        }else {
          count = count+1
          k = k - 1
        }
      }
    }

    count
  }
}