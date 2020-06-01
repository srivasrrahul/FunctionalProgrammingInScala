object Solution {
  def isPowerOfThree(n: Int): Boolean = {
    def itr(current : Int) : Boolean = {
      current match {
        case 0 => false
        case 1 => true
        case _ => {
          val mod3 = current % 3
          if (mod3 == 0) {
            itr(current/3)
          }else {
            false
          }
        }
      }
    }

    if (n < 0) {
      false
    }else {
      itr(n)
    }

  }

  def main(args: Array[String]): Unit = {
    println(isPowerOfThree(0))
  }
}