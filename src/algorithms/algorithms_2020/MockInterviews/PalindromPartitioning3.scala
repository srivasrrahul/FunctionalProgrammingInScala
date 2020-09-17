import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val j : Int,val p : Int)
object Solution {
  def palindromePartition(s: String, k: Int): Int = {
    val cache = new mutable.HashMap[Index,Option[List[List[Int]]]]()
    def partition(j : Int,p : Int) : Option[List[List[Int]]] = {
      //println(j + " " + p)
      val index = new Index(j,p)
      if (j >= s.length && p == 0) {
        Some(List())
      }else {
        if (j >= s.length) {
          None
        }else {
          if (p == 0) {
            None
          }else {
            if (cache.contains(index)) {
              println("Caceh hit")
              cache.get(index).get
            }else {
              val lsts = new ListBuffer[List[Int]]
              for (k <- j + 1 to s.length) {
                val pendingLsts = partition(k, p - 1)
                if (pendingLsts.isDefined) {
                  if (pendingLsts.get.isEmpty) {
                    lsts.append(List(j))
                  } else {
                    for (pendingLst <- pendingLsts.get) {
                      lsts.append(j :: pendingLst)
                    }
                  }

                }
              }

              if (lsts.size > 0) {
                val retValue = Some(lsts.toList)
                cache += ((index, retValue))
                retValue
              } else {
                cache += ((index, None))
                None
              }
            }
          }
        }
      }
    }

    def makePalindrome(b : Int,e : Int) : Int = {
      //println(b + " " + e)
      def itr(j : Int,k : Int) : Int = {
        if (j == k || j == k-1) {
          if (s(j) == s(k)) {
            0
          }else {
            1
          }
        }else {
          var cost = 0
          if (s(j) != s(k)) {
            cost = 1
          }

          cost + itr(j+1,k-1)
        }
      }

      itr(b,e)
    }
    val lsts = partition(0,k)
    if (lsts.isDefined) {
      var globalCost = Int.MaxValue
      //println(lsts)

      for (lst <- lsts.get) {
        println(lst)
        var first = 0
        var tail = lst.tail
        var cost = 0
        while (tail.isEmpty == false) {
          val (b,e) = (first,tail.head-1)
          cost = cost + makePalindrome(b,e)
          first = tail.head
          tail = tail.tail
        }

        cost = cost + makePalindrome(lst.last,s.length-1)
        if (cost < globalCost) {
          globalCost = cost
        }
      }

      globalCost
    }else {
      0
    }
//    println(lsts)
//    0
  }

  def main(args: Array[String]): Unit = {
    println(palindromePartition("abc",2))
  }
}