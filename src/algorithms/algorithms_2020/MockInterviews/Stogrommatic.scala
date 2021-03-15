object Solution {
  def isStrobogrammatic(num: String): Boolean = {
    val strobMap = Map('0'->'0','1'->'1','6'->'9','8'->'8','9'->'6')

    val newStr = new StringBuilder
    for (ch <- num.reverse) {
      if (strobMap.contains(ch)) {
        newStr.append(strobMap.get(ch).get)
      }
    }

    //println(newStr.toString())
    newStr.toString() == num
  }

  def main(args: Array[String]): Unit = {
    println(isStrobogrammatic("69"))
  }
}