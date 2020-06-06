import scala.collection.mutable

object Solution {
  def findTheDifference(s: String, t: String): Char = {
    var res = 0
    for (ch <- s) {
      res = res ^ ch.toInt
    }

    for (ch <- t) {
      res = res ^ ch.toInt
    }

    //println(res)
    res.toChar


  }

  def main(args: Array[String]): Unit = {
    println(findTheDifference("abc","abcd"))
  }
}