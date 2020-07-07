object Solution {
  def maxVowels(s: String, t: Int): Int = {
    val vowelsArr = new Array[Int](s.length)

    for (j <- 0 to s.length-1) {
      s(j) match {
        case 'a' | 'e' | 'i' | 'o' | 'u' => {
          if (j == 0) {
            vowelsArr(j) = 1
          }else {
            vowelsArr(j) = vowelsArr(j-1) + 1
          }

        }
        case _ => {
          if (j == 0) {
            vowelsArr(j) = 0
          }else {
            vowelsArr(j) = vowelsArr(j-1)
          }
        }
      }
    }

    //println(vowelsArr.mkString(","))
    var maxCount = Int.MinValue
    for (j <- 0 to s.length-t) {
      val k = j+t-1
      //println(k + " " + j)
      var count = vowelsArr(k) - vowelsArr(j)
      if (Set('a','e','i','o','i','u').contains(s(j))) {
        count = count + 1
      }
      if (count > maxCount) {
        maxCount = count
      }


    }

    maxCount
  }

  def main(args: Array[String]): Unit = {
    println(maxVowels("abciiidef",3))
  }
}