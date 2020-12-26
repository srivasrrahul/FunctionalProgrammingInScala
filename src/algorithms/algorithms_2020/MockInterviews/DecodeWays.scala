import scala.collection.mutable

object Solution {
  def numDecodings(s: String): Int = {
    val cache = new mutable.HashMap[Int,Option[Int]]()
    def itr(j : Int) : Option[Int] = {
      if (j == s.length-1) {
        if (s(j).asDigit == 0) {
          None
        }else {
          Some(1)
        }
      }else {
        if (s(j).asDigit == 0) {
          None
        }else {
          if (cache.contains(j)) {
            cache.get(j).get
          } else {
            //Take 1 and take 2
            var nextTwo: Option[Int] = None
            val nextTwoString = s.substring(j, j + 2)
            if (nextTwoString.toInt >= 1 && nextTwoString.toInt <= 26) {
              if (j + 1 == s.length - 1) {
                nextTwo = Some(1)
              } else {
                nextTwo = itr(j + 2)
              }
            }

            var nextOne: Option[Int] = None
            if (s(j).asDigit != 0) {
              nextOne = itr(j + 1)
            }

            var res : Option[Int] = None
            (nextOne, nextTwo) match {
              case (None, None) => {
                res = None
              }
              case (_, None) => {
                res = nextOne
              }
              case (None, _) => {
                res= nextTwo
              }
              case (_, _) =>  {
                res = Some(nextOne.get + nextTwo.get)
              }
            }

            cache += ((j,res))
            res
          }
        }
      }
    }

    val res = itr(0)
    if (res.isDefined) {
      res.get
    }else {
      0
    }
  }

  def main(args: Array[String]): Unit = {
    println(numDecodings("1234"))
  }
}