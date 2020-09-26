import scala.collection.mutable

case class Index(val l : Int,val pl :  scala.collection.immutable.TreeMap[Int,scala.collection.immutable.TreeSet[Int]] )
object Solution {
  def numMusicPlaylists(N: Int, L: Int, K: Int): Int = {
    val cache = new mutable.HashMap[Index,Option[Int]]()
    def itr(l : Int,playList : scala.collection.immutable.TreeMap[Int,scala.collection.immutable.TreeSet[Int]]) : Option[Int] = { //Tree Map is songId and index

      if (l == L+1) {
        //No need to add
        if (playList.size == N) {
          Some(1)
        }else {
          None
        }
      }else {
        val index = new Index(l,playList)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          var count = 0
          for (j <- 1 to N) {
            val earlierIdSet = playList.get(j)
            if (earlierIdSet.isDefined) {
              if (earlierIdSet.get.rangeFrom(l - K).size > 0) {
                //Can't add
              } else {
                val newSet = earlierIdSet.get.+(l)
                val c = itr(l + 1, playList.+((j, newSet)))
                if (c.isDefined) {
                  count = (count + c.get) % (Math.pow(10, 9) + 7).toInt
                }
              }
            } else {
              val c = itr(l + 1, playList.+((j, new scala.collection.immutable.TreeSet[Int] ++ Set(l))))
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

    val count = itr(1,new scala.collection.immutable.TreeMap[Int,scala.collection.immutable.TreeSet[Int]]())
    if (count.isDefined) {
      count.get
    }else {
      -1
    }
  }

  def main(args: Array[String]): Unit = {

    println(numMusicPlaylists(2,3,1))
  }
}