import scala.collection.mutable

class HitCounter() {

  /** Initialize your data structure here. */
    //value is count
  val treeMap = new mutable.TreeMap[Int,Int]()


  /** Record a hit.
        @param timestamp - The current timestamp (in seconds granularity). */
  def hit(timestamp: Int) : Unit = {
    treeMap.get(timestamp) match {
      case None => {
        treeMap += ((timestamp,1))
      }
      case Some(existingCount) => {
        treeMap += ((timestamp,existingCount+1))
      }
    }
  }

  /** Return the number of hits in the past 5 minutes.
        @param timestamp - The current timestamp (in seconds granularity). */
  def getHits(timestamp: Int): Int = {
    var counter = 0
    //println(treeMap.range(timestamp-300,timestamp+1))
    for (itr <- treeMap.range(timestamp-299,timestamp+1)) {
      counter = counter + itr._2
    }

    counter
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val hitCounter = new HitCounter
    hitCounter.hit(1)
    hitCounter.hit(2)
    hitCounter.hit(3)
    println(hitCounter.getHits(4))
    hitCounter.hit(300)
    println(hitCounter.getHits(300))
    println(hitCounter.getHits(301))
    hitCounter.hit(310)
    hitCounter.hit(600)

    //println(hitCounter.getHits(600))
  }
}