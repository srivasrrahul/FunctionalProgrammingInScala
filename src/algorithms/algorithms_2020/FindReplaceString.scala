import scala.collection.mutable

import scala.util.control.Breaks._
object Solution {
  def findReplaceString(S: String, indexes: Array[Int], sources: Array[String], targets: Array[String]): String = {
    val indexTree = new mutable.TreeSet[Int]()
    val needleReplace = new mutable.HashMap[Int,(String,String)]()
    var i = 0
    for (index <- indexes) {
      indexTree.add(index)
      needleReplace += ((index,(sources(i),targets(i))))
      i = i + 1
    }

    var j = 0
    val updatedStr = new StringBuilder
    while (j < S.length) {
      if (indexTree.contains(j)) {
        //check if source match the next
        val (needle,replaceStr) = needleReplace.get(j).get
        val whereAmI = j
        breakable {
          for (k <- 0 to needle.length - 1) {
            if (S.charAt(j) == needle.charAt(k)) {
              if (k == needle.length-1) {
                //Completely matched
                updatedStr.append(replaceStr)
                j = j + 1
              }else {
                j = j + 1
              }
            } else {
              //They don't match at all
              //at least where was I
              updatedStr.append(S.charAt(whereAmI))
              j = whereAmI + 1
              break()
            }
          }
        }


      }else {
        updatedStr.append(S.charAt(j))
        j = j + 1
      }
    }

    updatedStr.toString()
  }

  def main(args: Array[String]): Unit = {
    println(findReplaceString("abcd",Array(0,2),Array("a","cd"),Array("eee","ffff")))
    println(findReplaceString("abcd",Array(0,2),Array("ab","ec"),Array("eee","ffff")))
  }
}