object Solution {
  def addBinary(a: String, b: String): String = {
    if (a.length < b.length) {
      addBinary(b,a)
    }else {

      var indexA = a.length-1
      var indexB = b.length-1
      var carry = 0

      val result = new StringBuilder

      while (indexA >= 0 && indexB >= 0) {
        val currA = a(indexA).asDigit
        val currB = b(indexB).asDigit

        //println(currA + " " + currB)
        (currA,currB) match {
          case (0,0) => {
            if (carry == 0) {
              result.append(0)
            }else {
              result.append(1)
              carry = 0
            }
          }
          case (0,1) | (1,0) => {
            if (carry == 1) {
              result.append(0)
              carry = 1
            }else {
              result.append(1)
              carry = 0
            }
          }
          case (1,1) => {
            if (carry == 1) {
              result.append(1)
              carry = 1
            }else {
              result.append(0)
              carry = 1
            }
          }
          case (_,_) => {

          }
        }

        indexA = indexA - 1
        indexB = indexB - 1

      }

      while (indexA >= 0) {
        val currA = a(indexA).asDigit
        currA match {
          case 0 => {
            if (carry == 1) {
              result.append(1)
              carry = 0
            }else {
              result.append(0)
              carry = 0
            }
          }
          case 1 => {
            if (carry == 1) {
              result.append(0)
              carry = 1
            }else {
              result.append(1)
              carry = 0
            }
          }
          case _ => {

          }
        }

        indexA = indexA - 1
      }

      if (carry == 1) {
        result.append(1)
      }

      //println(" " + result.toString())
      result.reverse.toString()
    }


  }

  def main(args: Array[String]): Unit = {
    println(addBinary("11","110"))
  }
}