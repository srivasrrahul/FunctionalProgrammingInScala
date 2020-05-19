object Solution {

  def lcsDP(text1: String, text2: String) : Int = {
    val rowSize = text1.length
    val colSize = text2.length
    val matrix = Array.ofDim[Int](rowSize,colSize)

    var ifSet = false
    for (j <- 0 to colSize-1) {
      if (text2(j) == text1(0)) {
        matrix(0)(j) = 1
        ifSet = true
      }else {
        if (ifSet == true) {
          matrix(0)(j) = 1
        }
      }
    }

    ifSet = false
    for (j <- 0 to rowSize-1) {
      if (text1(j) == text2(0)) {
        matrix(j)(0) = 1
        ifSet = true
      }else {
        if (ifSet == true) {
          matrix(j)(0) = 1
        }
      }
    }

    for (j <- 1 to rowSize-1) {
      for (k <- 1 to colSize-1) {
        var diff = 1
        if (text1(j) != text2(k)) {
          diff = 0
        }

        val option1 = matrix(j-1)(k)
        val option2 = matrix(j)(k-1)
        val option3 = matrix(j-1)(k-1) + diff

        matrix(j)(k) = Array(option1,option2,option3).max

      }
    }

    matrix(rowSize-1)(colSize-1)
  }

  def longestCommonSubsequence(text1: String, text2: String): Int = {
//    def itr(xIndex : Int,yIndex : Int) : Int = {
//      //println(xIndex + " " + yIndex)
//      if (xIndex < 0 || yIndex < 0) {
//        0
//      }else {
//        val option1 = itr(xIndex-1,yIndex)
//        val option2 = itr(xIndex,yIndex-1)
//        var diff = 1
//        if (text1(xIndex) != text2(yIndex)) {
//          diff = 0
//        }
//
//        val option3 = diff + itr(xIndex-1,yIndex-1)
//        Array(option1,option2,option3).max
//      }
//    }
//
//    itr(text1.length-1,text2.length-1)
    lcsDP(text1,text2)
  }



  def main(args: Array[String]): Unit = {
    println(longestCommonSubsequence("abasdasdcde","ace"))
  }
}