import scala.collection.mutable

case class Index(val cutBeginIndex : Int,val cutEndIndex : Int,val stickBegin : Int,val stickEnd : Int)
object Solution {
  def minCost(n: Int, cuts: Array[Int]): Int = {
    cuts.sortInPlace()
    val cache = new mutable.HashMap[Index,Int]()
    def itr(cutBeginIndex : Int,cutEndIndex : Int,stickBegin : Int,stickEnd : Int) : Int = {
      //println(cutBeginIndex + " " + cutEndIndex + " " + stickBegin + " " + stickEnd)
      if (cutBeginIndex > cutEndIndex) {
        0
      }else {
        val index = new Index(cutBeginIndex,cutEndIndex,stickBegin,stickEnd)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          val stickLen = stickEnd - stickBegin
          var localMinCost = Int.MaxValue
          var minCutIndex = 0
          for (p <- cutBeginIndex to cutEndIndex) {
            val c1 = itr(cutBeginIndex, p - 1, stickBegin, cuts(p)) + itr(p + 1, cutEndIndex, cuts(p), stickEnd)
            if (c1 < localMinCost) {
              minCutIndex = cuts(p)
              localMinCost = c1
            }
          }

          //println("Min Cut at " + minCutIndex + " for " + stickBegin +  " " + stickEnd + " cost = " + (localMinCost + stickLen))
          cache += ((index,(localMinCost + stickLen)))
          localMinCost + stickLen
        }
      }
    }

    itr(0,cuts.length-1,0,n)
  }

  def main(args: Array[String]): Unit = {
    println(minCost(9,Array(5,6,1,4,2)))
  }
}