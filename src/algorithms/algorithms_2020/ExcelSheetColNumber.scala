object Solution {
  def titleToNumber(s: String): Int = {
    var num = 0
    for (j <- 0 to s.length-1) {
      val currentChar = s(j)
      currentChar match {
        case 'Z' => {
          if (num == 0) {
            num = 26
          }else {
            num = num * 26 + 26
          }
        }
        case _ => {
          num = num*26 + (currentChar - 'A' + 1)
        }
      }
    }

    num

  }

  def main(args: Array[String]): Unit = {
    println(titleToNumber("AGT"))

  }
}