import scala.collection.mutable
import util.control.Breaks._

object Solution {
//  def wordBreak(s: String, wordDict: List[String]): Boolean = {
//    val wordSet = new mutable.HashSet[String]()
//    wordDict.foreach(word => {
//      wordSet.add(word)
//    })
//
//    def itr(stringIndex : Int) : Boolean = {
//      if (stringIndex >= s.length) {
//        false
//      }else {
//        //println(stringIndex)
//        var result = false
//        breakable {
//          for (j <- stringIndex to s.length - 1) {
//            val s1 = s.substring(stringIndex, j+1)
//            val s2 = s.substring(j+1)
//
//            //println("s1 = " + s1  + " s2 " + s2)
//
//            if (wordSet.contains(s1) && wordSet.contains(s2)) {
//              result = true
//              break
//            }else {
//              if (wordSet.contains(s1)) {
//                if (itr(j+1) == true) {
//                  result = true
//                  break
//                }
//              }
//            }
//          }
//        }
//
//        if (result == false) {
//          val subStr = s.substring(stringIndex)
//          if (wordSet.contains(subStr)) {
//            result = true
//          }
//        }
//
//
//        result
//      }
//    }
//
//    itr(0)
//  }

  def wordBreak(s : String,wordDict: List[String]): Boolean = {
    val size = s.length
    val matrix = Array.ofDim[Boolean](size,size)

    for (j <- 0 to size-1) {
      for (k <- 0 to size-1) {
        matrix(j)(k) = false
      }
    }

    val wordSet = new mutable.HashSet[String]()
    wordDict.foreach(word => {
      wordSet.add(word)
    })

    for (j <- 0 to size-1) {
      if (wordSet.contains(s(j).toString)) {
        matrix(j)(j) = true
      }
    }

    var j = 0
    var k = 0

    var r = 1

    breakable {
      while (true) {
        while (j <= size - 1 && k <= size - 1) {
          //println("j = " + j + " k = " + k)

          val subStr = s.substring(j, k + 1)

          if (wordSet.contains(subStr)) {
            matrix(j)(k) = true
          }else {
            breakable {
              for (x <- j to k) {
                if (matrix(j)(x) && matrix(x + 1)(k)) {
                  matrix(j)(k) = true
                  break
                }
              }
            }
          }

          //println("matrix(" + j + ")(" + k + ")" + "=" + matrix(j)(k))
          j = j + 1
          k = k + 1

        }

        j = 0
        k = j + r
        r = r + 1

        if (k > size) {
          break
        }


      }


    }

    matrix(0)(size-1)

  }

  def main(args: Array[String]): Unit = {
    println(wordBreak("catsandog",List("cats", "dog", "sand", "and", "cat")))
  }
}