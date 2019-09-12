object Solution {
  def toInt(s: String):Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

  def reverse(x: Int): Int = {
    val absX = Math.abs(x)
    val absXRev = toInt(absX.toString.reverse)
    absXRev match {
      case Some(y) => {
        if (x < 0) {
          y*(-1)
        }else {
          y
        }
      }
      case None => {
        0
      }
    }


  }

  def main(args: Array[String]): Unit = {
    println(reverse(120))
  }
}