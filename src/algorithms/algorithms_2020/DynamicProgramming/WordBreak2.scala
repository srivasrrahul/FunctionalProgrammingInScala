import scala.collection.mutable.ListBuffer

object Solution {
  def wordBreak(s: String, words: List[String]): List[String] = {
    val wordsDict = words.toSet
    val matrix = Array.ofDim[List[List[String]]](s.length+1,s.length+1)
    for (j <- 0 to s.length-1) {
      for (k <- j to s.length-1) {
        //val subStr = s.substring(j,k+1)
        matrix(j)(k) = List()
//        if (wordsDict.contains(subStr)) {
//          println(" found " + j + " " + k)
//          matrix(j)(k) = List(List(subStr))
//        }else {
//          matrix(j)(k) = List()
//        }

      }
    }

//    for (j <- 0 to s.length-1) {
//      println(matrix(j).mkString(","))
//    }
//    println(matrix(0)(3))
//    println(matrix(4)(6))

    var j = 0
    var current = 0
    var k = current

    while (j < s.length && k < s.length) {
      val combinedLst = new ListBuffer[List[String]]

      for (p <- j to k-1) {

        val left = matrix(j)(p)
        val right = matrix(p+1)(k)

        if (left.isEmpty == false && right.isEmpty == false) {
          //Both are valid

          val cross = left.flatMap(l => right.map(r => l ++ r))
          combinedLst.appendAll(cross)
        }
      }

      val subStr = s.substring(j,k+1)
      if (wordsDict.contains(subStr)) {
        combinedLst.append(List(subStr))
      }

      matrix(j)(k) = combinedLst.toList ++ matrix(j)(k)
//      if (combinedLst.length > 0) {
//        matrix(j)(k) = combinedLst.toList ++ matrix(j)(k)
//      }


      j = j+1
      k = k+1

      if (k >= s.length) {
        current = current+1
        j = 0
        k = current
      }
    }

    val finalLsts = matrix(0)(s.length-1)
    val strLst = new ListBuffer[String]
    for (lst <- finalLsts) {
      strLst.append(lst.mkString(" "))
    }

    strLst.toList.toSet.toList
  }

  def main(args: Array[String]): Unit = {
    println(wordBreak("catsandog",List("cats", "dog", "sand", "and", "cat")))
  }
}