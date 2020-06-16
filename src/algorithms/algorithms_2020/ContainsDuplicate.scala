import scala.collection.mutable

object Solution {
  def containsNearbyDuplicate(nums: Array[Int], target: Int): Boolean = {
    val countIndex = new mutable.HashMap[Int,mutable.TreeSet[Int]]()

    var found = false
    for (j <- 0 to nums.length-1 if found == false) {
      val x = nums(j)
      countIndex.get(x) match {
        case None => {
          val set = new mutable.TreeSet[Int]()
          set.add(j)
          countIndex += ((x,set))
        }
        case Some(set) => {
          //if j is smaller
          //println(set)

          var left = j - target
          if (left < 0) {
            left = 0
          }

          val right = j + target+1
          if (set.range(left,right).size > 0) {
            found = true
          }

          set.add(j)
        }
      }
    }

    found




  }

  def main(args: Array[String]): Unit = {
    println(containsNearbyDuplicate(Array(99,99),2))
  }
}