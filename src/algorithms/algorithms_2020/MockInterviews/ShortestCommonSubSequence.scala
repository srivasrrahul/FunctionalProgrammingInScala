import scala.collection.mutable

case class Index(val x : Int,val y : Int)
object Solution {
  def shortestCommonSupersequence(str1: String, str2: String): String = {
    val arr = Array.ofDim[Int](str1.length,str2.length)
    var isSubStr  = false
    for (j <- 0 to str1.length-1) {
      if (str2(0) == str1(j)) {
        arr(j)(0) = j+1
        isSubStr = true
      }else {
        if (isSubStr) {
          arr(j)(0) = j+1
        }else {
          arr(j)(0) = j+2
        }
      }
    }

    isSubStr = false
    for (k <- 0 to str2.length-1) {
      if (str1(0) == str2(k)) {
        arr(0)(k) = k+1
        isSubStr = true
      }else {
        if (isSubStr) {
          arr(0)(k) = k+1
        }else {
          arr(0)(k) = k +2
        }
      }
    }

    for (j <- 1 to str1.length-1) {
      for (k <- 1 to str2.length-1) {
        if (str1(j) == str2(k)) {
          arr(j)(k) = arr(j-1)(k-1) + 1
        }else {
          arr(j)(k) = math.min(arr(j-1)(k),arr(j)(k-1)) + 1
        }
      }
    }

    val strBuilder = new StringBuilder
    var j = str1.length-1
    var k = str2.length-1

    while (j != 0 && k != 0) {
      if (str1(j) != str2(k)) {
        //Move in lower direction
        if (arr(j-1)(k) < arr(j)(k-1)) {
          strBuilder.append(str1(j))
          j = j -1
        }else {
          strBuilder.append(str2(k))
          k = k -1
        }
      }else {
        strBuilder.append(str2(k))
        j = j - 1
        k = k - 1
      }
    }

    if (j == 0) {
      for (p <- k to 0 by -1) {
        strBuilder.append(str2(p))
      }

      if (arr(0)(k) == k+2) {
        strBuilder.append(str1(0))
      }

    }else {
      for (p <- j to 0 by -1) {
        strBuilder.append(str1(p))
      }

      if (arr(j)(0) == j+2) {
        strBuilder.append(str2(0))
      }
    }

    strBuilder.reverse.toString()
    //"abc"
  }

  def main(args: Array[String]): Unit = {
    println(shortestCommonSupersequence("abac","cab"))

  }
}