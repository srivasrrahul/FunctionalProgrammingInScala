import scala.collection.mutable.ListBuffer

object Solution {
  def partition(s: String): List[List[String]] = {
    def checkPalindrome(j : Int,k : Int) : Boolean = {
      if (j==k) {
        true
      }else {
        if (j == k-1) {
          s(j) == s(k)
        }else {
          s(j) == s(k) && checkPalindrome(j + 1, k - 1)
        }
      }
    }

    def itr(j : Int) : List[List[String]] = {

      if (j == s.length-1) {
        List(List(s.last.toString))
      }else {
        val res = new ListBuffer[List[String]]
        for (k <- j+1 to s.length-1) {
          val pending = itr(k)
          if (pending.isEmpty == false && checkPalindrome(j,k-1)) {
            val substr = s.substring(j,k)
            for (p <- pending) {
              res.append(substr::p)
            }
          }
        }

        if (checkPalindrome(j,s.length-1)) {
          res.append(List(s.substring(j,s.length)))
        }
        res.toList
      }
    }

    itr(0)
  }

  def main(args: Array[String]): Unit = {
    println(partition("aab"))
  }
}