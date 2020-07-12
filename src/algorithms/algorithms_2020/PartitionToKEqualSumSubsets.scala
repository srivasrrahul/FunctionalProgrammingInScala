import scala.collection.mutable

object Solution {
  def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean = {
    val cache = new mutable.HashMap[List[Int],Boolean]()
    def itr(currenIndex : Int,lst : Array[Int]): Boolean = {
      if (currenIndex == nums.length) {
        //println(lst.mkString(","))
        lst.toSet.size == 1
      }else {
        val key = lst.toList
        if (cache.contains(key)) {
          cache.get(key).get
        }else {
          var retValue = false
          for (j <- 0 to k - 1 if retValue == false) {
            val newLst = lst.clone()
            newLst(j) = newLst(j) + nums(currenIndex)
            val localRetValue = itr(currenIndex + 1, newLst)
            if (localRetValue == true) {
              retValue = true
            }
          }

          cache += ((lst,retValue))
          retValue
        }
      }
    }

    val arr = new Array[Int](k)
    for (j <- 0 to arr.length-1) {
      arr(j) = 0
    }
    itr(0,arr)
  }

  def main(args: Array[String]): Unit = {
    println(canPartitionKSubsets(Array(4, 3, 2, 3, 5, 2, 1),4))
  }
}