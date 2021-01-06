object Solution {
  def findElement(r : Range,index : Int) : Int = {
    //println(r + " " + index)
    val first = r.head
    (first-1)+index
  }
  def findKthPositive(arr: Array[Int], k: Int): Int = {
    var expected = 1
    var finalIndex = k
    //var totalMissing = 0
    var found = -1
    for (x <- arr if found == -1) {
      //println("For x " + x + " " + expected + " " + finalIndex)
      if (x==expected) {
        expected = x+1
      }else {
        val currentMissing = x-expected
        //totalMissing = totalMissing + currentMissing
        if (finalIndex-currentMissing > 0) {
          finalIndex = finalIndex-currentMissing
        }else {
          val r = Range(expected,x)
          found = findElement(r,finalIndex)
        }

        expected = x+1

      }
    }

    if (found == -1) {
      val r = Range(expected,Int.MaxValue)
      found = findElement(r,finalIndex)
    }

    found
  }

  def main(args: Array[String]): Unit = {
    println(findKthPositive(Array(2,3,4,7,11),5))
  }
}