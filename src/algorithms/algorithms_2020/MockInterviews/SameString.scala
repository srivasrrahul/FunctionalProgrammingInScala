object Solution {
  def arrayStringsAreEqual(word1: Array[String], word2: Array[String]): Boolean = {
    var j1 = 0
    var k1 = 0

    var j2 = 0
    var k2 = 0

    var isEqual = true
    //println(word1.length + " " + word1.last.length)
    //println(word2.length + " " + word2.last.length)
    while (j1 < word1.length && j2 < word2.length && isEqual == true) {
      val w1 = word1(j1)
      val w2 = word2(j2)

      if (w1(k1) != w2(k2)) {
        isEqual = false
      }else {
        //println(w1(k1))
        k1 = k1 + 1
        if (k1 >= w1.length) {
          k1 = 0
          j1 = j1+1
        }

        k2 = k2+1
        if (k2 >= w2.length) {
          k2 = 0
          j2 = j2 + 1
        }
      }
    }

    //println(isEqual + " " + j1 + " " + k1 + " " + j2 + " " + k2)
    isEqual && j1 >= word1.length && j2 >= word2.length
  }

  def main(args: Array[String]): Unit = {
    println(arrayStringsAreEqual(Array("abc"),Array("a","bcd")))
  }
}