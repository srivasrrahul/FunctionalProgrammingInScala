object Solution {
  def convertToTitle(n: Int): String = {
    var first = n
    val numStr = new StringBuilder
    while (first > 26) {
      val pending = first / 26
      val mod = first % 26
      //println(mod)
      if (mod == 0) {
        numStr.append('Z')
        first = pending-1
      }else {
        numStr.append(('A' + mod - 1).toChar)
        first = pending
      }

    }

    //println(first)
    if (first == 0) {
      numStr.append('Z')
    }else {
      numStr.append(('A' + first - 1).toChar)
    }
    numStr.reverse.toString()
  }

  def main(args: Array[String]): Unit = {
    println(convertToTitle(701))
  }
}