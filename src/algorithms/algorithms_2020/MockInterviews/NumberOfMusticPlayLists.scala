import scala.collection.mutable

case class Index(val l : Int,val pl :  Set[Int] )
object Solution {
  def numMusicPlaylists(N: Int, L: Int, K: Int): Int = {
    val cache = new mutable.HashMap[Index,Option[Int]]()
    def itr(l : Int,playList : scala.collection.immutable.Map[Int,Int]) : Option[Int] = { //Tree Map is songId and index

      if (l == L+1) {
        //No need to add

        //println(playList)
        if (playList.size == N) {
          //println(playList)
          Some(1)
        }else {
          None
        }
      }else {
        val index = new Index(l,playList.keys.toSet)
        if (cache.contains(index)) {
          //println("test")
          cache.get(index).get
        }else {
          var count = 0
          for (j <- 1 to N) {
            val lastPlayedIndex = playList.getOrElse(j,-1)
            if (lastPlayedIndex != -1) {
              if (l - K-1 < lastPlayedIndex) {
                //Can't add
              } else {
                val c = itr(l + 1, playList.+((j, l)))
                if (c.isDefined) {
                  count = (count + c.get) % (Math.pow(10, 9) + 7).toInt
                }
              }
            } else {
              val c = itr(l + 1, playList.+((j,l)))
              if (c.isDefined) {
                count = (count + c.get) % (Math.pow(10, 9) + 7).toInt
              }
            }

          }

          if (count > 0) {
            cache += ((index,Some(count)))
            Some(count)
          } else {
            cache += ((index,None))
            None
          }
        }
      }
    }

    val count = itr(1,scala.collection.immutable.Map[Int,Int]())
    if (count.isDefined) {
      count.get
    }else {
      -1
    }
  }

  def main(args: Array[String]): Unit = {

    println(numMusicPlaylists(3,3,1))
  }
}