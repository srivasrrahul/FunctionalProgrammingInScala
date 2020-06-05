import scala.collection.mutable

object Solution {
  def threeSumSmaller(nums: Array[Int], target: Int): Int = {
    nums.sortInPlace()

    var count = 0
    for (i <- 0 to nums.length-3) {
      val localTarget = target - nums(i)
      val numCount = new mutable.TreeMap[Int,Int]()
      numCount += ((nums(i+1),1))

      for (j <- i+2 to nums.length-1) {
        val current = nums(j)
        val pending = localTarget - current
        val pendingTree = numCount.rangeUntil(pending)
        if (pendingTree.isEmpty == false) {
          for ((_,localCount) <- pendingTree) {
            count = count + localCount
          }
        }

        val defaultCount = numCount.getOrElseUpdate(current,0)
        numCount += ((current,defaultCount+1))
      }

    }

    count
  }

  def main(args: Array[String]): Unit = {
    println(threeSumSmaller(Array(2,0,0,2,-2),2))
  }
}