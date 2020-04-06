import util.control.Breaks._

object Solution {
  def checkInclusion(s1: String, s2: String): Boolean = {
    val s1Sorted = s1.toSeq.sorted.unwrap
    val s1Length = s1.length

    var j = 0
    var retValue = false
    breakable {
      while (j < s2.length && j + s1Length <= s2.length) {
        val sortedSubStr = s2.substring(j,j + s1Length).toSeq.sorted.unwrap
        //println("Sorted sub string "+ sortedSubStr)
        if (sortedSubStr == s1Sorted) {
          retValue = true
          break
        }

        j = j + 1
      }
    }

    retValue

  }

  def main(args: Array[String]): Unit = {
    println(checkInclusion("adc","dcda"))
  }
}