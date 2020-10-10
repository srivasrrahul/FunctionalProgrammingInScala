import scala.collection.mutable

object Solution {
  def longestSubarray(nums: Array[Int], limit: Int): Int = {
    val slidingWindow = new mutable.TreeMap[Int,(Int,Int)]() //count and last seen index
    slidingWindow += ((nums(0), (1,0)))
    var maxCount = 1
    var count = 1

    for (j <- 1 to nums.length-1) {
      if (slidingWindow.contains(nums(j))) {
        val (defCount,defIndex) = slidingWindow.get(nums(j)).get
        slidingWindow += ((nums(j),(defCount+1,j)))
      }else {
        val min = slidingWindow.head._1
        val max = slidingWindow.last._1
        if (nums(j) >= min && nums(j) <= max) {
          slidingWindow += ((nums(j),(1,j)))
        }else {
          var maxIndex = -1
          for ((key,(_,lastIndex)) <- slidingWindow) {
            if (math.abs(nums(j)-key) > limit) {
              maxIndex = math.max(maxIndex,lastIndex)
            }
          }

          val offendingKeys = new mutable.HashSet[Int]()
          for ((key,(_,lastIndex)) <- slidingWindow) {
            if (lastIndex <= maxIndex) {
              offendingKeys.add(key)
            }
          }

          for (offendingKey <- offendingKeys) {
            slidingWindow.remove(offendingKey)
          }

          slidingWindow += ((nums(j),(1,j)))

        }

      }

      var localCount = 0
      for ((_,(count,_)) <- slidingWindow) {
        localCount = localCount + count
      }

      maxCount = math.max(localCount,maxCount)
    }

    maxCount
  }
}