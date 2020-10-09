import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val j : Int,val k : Int)
object Solution {
  def partition(s: String): List[List[String]] = {
    def isPalindrome(j : Int,k : Int) : Boolean = {
      if (j == k) {
        true
      }else {
        if (s(j) == s(k)) {
          if (j+1 == k) {
            true
          }else {
            isPalindrome(j+1,k-1)
          }
        }else {
          false
        }
      }
    }

    val cache = new mutable.HashMap[Index,List[List[String]]]()
    def itr(j : Int,k : Int) : List[List[String]] = {
      if (j == k) {
        List(List(s(j).toString))
      }else {
        val index = new Index(j,k)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          val retValue = new ListBuffer[List[String]]
          if (isPalindrome(j, k)) {
            retValue.append(List(s.substring(j, k + 1)))
          }

          for (p <- j to k - 1) {
            if (isPalindrome(j, p)) {
              val pendingLst = itr(p + 1, k)
              if (pendingLst.isEmpty == false) {
                val subStr = s.substring(j, p + 1)
                for (pending <- pendingLst) {
                  retValue.append(subStr :: pending)
                }
              }
            }
          }

          val lst = retValue.toList
          cache += ((index,lst))
          lst
        }
      }
    }

    if (s.isEmpty) {
      List(List())
    }else {
      itr(0, s.length - 1)
    }

  }
}