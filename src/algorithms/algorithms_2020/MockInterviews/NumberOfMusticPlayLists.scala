import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val l : Int,val pl :  Set[Int] )
object Solution {
  def numMusicPlaylists(N: Int, L: Int, K: Int): Int = {

    var arr = new mutable.ArrayBuffer[List[List[Int]]]()
    for (j <- 0 to N-1) {
      arr.append(List(List(j)))
    }

    for (l <- 2 to L) {
      var newArr = new mutable.ArrayBuffer[List[List[Int]]]()
      for (j <- 0 to N-1) {
        val lstBuffer = new ListBuffer[List[Int]]
        for (k <- 0 to N-1) {
          if (j != k) {
            val lsts = arr(k)
            for (lst <- lsts) {
              var k = 1
              var canAdded = true
              for (ls <- lst if k <= K) {
                if (ls == j) {
                  canAdded = false
                }

                k = k + 1
              }

              if (canAdded) {
                lstBuffer.append(j :: lst)
              }
            }
          }
        }

        newArr.append(lstBuffer.toList)
      }

      println(newArr)

      arr.clear()
      arr = newArr
    }


    var count = 0
    for (lsts <- arr) {
      for (lst <- lsts) {
        if (lst.toSet.size == N) {
          count = count + 1
        }
      }
    }

    count
  }

  def main(args: Array[String]): Unit = {

    println(numMusicPlaylists(3,3,2))
  }
}