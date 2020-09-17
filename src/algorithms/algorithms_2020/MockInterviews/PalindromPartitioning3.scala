import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val j : Int,val p : Int)
object Solution {


  def palindromePartition(s: String, k: Int): Int = {
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


    val cache = new mutable.HashMap[Index,Option[List[Int]]]()
    def partition(j : Int,p : Int) : Option[List[Int]] = {
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
              //println("Caceh hit")
              cache.get(index).get
            }else {
              val lsts = new ListBuffer[Int]
              for (k <- j + 1 to s.length) {
                val costs = partition(k, p - 1)

                if (costs.isDefined) {

                  val localCost = makePalindrome(j,k-1)

                  if (costs.get.isEmpty) {
                    lsts.append(localCost)
                  } else {

                    for (cost <- costs.get) {
                      lsts.append(localCost + cost)
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


    val lsts = partition(0,k)
    if (lsts.isDefined) {
      lsts.get.min
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