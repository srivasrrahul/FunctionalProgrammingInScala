import scala.collection.mutable

object Solution {
  def maxCoins(nums: Array[Int]): Int = {
    def itr(present : scala.collection.immutable.TreeSet[Int]) : Int = {
      val cache = new mutable.HashMap[scala.collection.immutable.TreeSet[Int],Int]()
      if (present.size == 1) {
        //last one
        nums(present.head)
      }else {
        if (cache.contains(present)) {
          cache.get(present).get
        }else {
          var maxCoin = 0
          for (j <- present) {
            val left = present.rangeUntil(j)
            val right = present.rangeFrom(j + 1)

            var leftSize = 1
            if (left.size > 0) {
              leftSize = nums(left.last)
            }

            var rightSize = 1
            if (right.size > 0) {
              rightSize = nums(right.head)
            }

            val coins = (leftSize * rightSize * nums(j)) + itr(present.-(j))
            if (coins > maxCoin) {
              maxCoin = coins
            }
          }

          cache += ((present,maxCoin))
          maxCoin
        }


      }
    }

    val present = new scala.collection.immutable.TreeSet[Int]()

    itr(present.++(Range(0,nums.length).toList))
  }
}