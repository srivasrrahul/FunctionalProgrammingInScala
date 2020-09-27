import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val l : Int,val pl :  Set[Int] )
object Solution {
  def numMusicPlaylists(N: Int, L: Int, K: Int): Int = {

    def itr(l : Int,n : Int) : List[List[Int]] = {
      //println(l + "  " + n)
      if (l == 1) {
        List(List(n))
      }else {
        var count = 0
        val lstBuffer = new ListBuffer[List[Int]]
        for (j <- 1 to N) {
          if (j != n) {
            val lsts = itr(l-1,j)
            //println(l + " " + n + " " + (l-1) + " " + j + " " + lsts)

            for (lst <- lsts) {
              var k = 1
              var canAdded = true
              for (ls <- lst if k <= K) {
                if (ls == n) {
                  canAdded = false
                }
                k = k + 1
              }

              if (canAdded) {
                lstBuffer.append(n :: lst)
              }

            }


          }
        }

        lstBuffer.toList
      }
    }

    var count = 0
    for (j <- 1 to N) {
      val lsts  = itr(L,j)
      println(lsts)
      for (lst <- lsts) {
        if (lst.toSet.size == N) {
          count = count + 1
        }
      }
    }

    count
  }

  def main(args: Array[String]): Unit = {

    println(numMusicPlaylists(3,3,1))
  }
}