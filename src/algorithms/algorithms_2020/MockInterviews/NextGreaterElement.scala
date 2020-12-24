object Solution {
  def nextGreaterElement(n: Int): Int = {
    val s = n.toString

    def findLessThanIndex(last : Int) : Int = {
      var lessThanIndex = -1
      var j = last-1
      while (j >= 0 && lessThanIndex == -1) {
        if (s(j).toInt < s(last).toInt) {
          lessThanIndex = j
        }

        j = j - 1
      }

      lessThanIndex
    }

    var splitIndex = -1
    var lessThan = -1
    for (k <- s.length-1 to 0 by -1) {
      val lessThanIndex = findLessThanIndex(k)
      if (lessThanIndex > lessThan)  {
        lessThan = lessThanIndex
        splitIndex = k
      }
    }

    if (splitIndex != -1) {
      //println(splitIndex + " " + lessThan)
      val noChange = s.substring(0,lessThan)
      val totalPending = s(splitIndex).toString
      val next = s.substring(lessThan+1,splitIndex) + s(lessThan).toString + s.substring(splitIndex+1)
      val res = (noChange ++ totalPending + next.toSeq.sorted.unwrap).toLong
      if (res > Int.MaxValue) {
        -1
      }else {
        res.toInt
      }
    }else {
      -1
    }
  }

  def main(args: Array[String]): Unit = {
    var dig = 230241
    for (j <- 0 to 30 if dig >= 1) {
      val res = nextGreaterElement(dig)
      dig = res

      println(res)
    }

  }
}