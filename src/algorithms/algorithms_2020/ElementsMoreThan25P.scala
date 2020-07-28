import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def findSpecialInteger(arr: Array[Int]): Int = {
    val countMap = new mutable.TreeMap[Int,mutable.HashSet[Int]]
    val mapCount = new mutable.HashMap[Int,Int]()
    for (j <- 0 to arr.length-1) {
      val defaultCount = mapCount.getOrElse(arr(j),0)
      mapCount += ((arr(j),defaultCount+1))

      val latestCount = defaultCount+1

      val defaultSet = countMap.getOrElseUpdate(defaultCount,new mutable.HashSet[Int]())
      defaultSet.remove(defaultCount)

      val latestSet = countMap.getOrElseUpdate(latestCount,new mutable.HashSet[Int]())
      latestSet.add(arr(j))

    }

    countMap.last._2.head
  }
}