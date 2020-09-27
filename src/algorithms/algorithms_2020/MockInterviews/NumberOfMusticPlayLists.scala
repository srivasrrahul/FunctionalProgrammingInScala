import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index(val l : Int,val n :  Int )
object Solution {
  def numMusicPlaylists(N: Int, L: Int, K: Int): Int = {

    val cache = new mutable.HashMap[Index,Long]()
    def itr(l : Int, n : Int) : Long = {
      if (l==1) {
        if (n == 1) {
          N.toLong
        }else {
          0.toLong
        }
      }else {
        val index = new Index(l,n)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          val c1 = itr(l - 1, n - 1) * (N - (n - 1))
          val c2 = itr(l - 1, n) * math.max(n - K, 0)
          val retValue = (c1 + c2) % ((Math.pow(10, 9) + 7).toInt)
          cache += ((index,retValue))
          retValue
        }
      }
    }

    itr(L,N).toInt

  }

  def main(args: Array[String]): Unit = {

    println(numMusicPlaylists(3,3,2))
  }
}