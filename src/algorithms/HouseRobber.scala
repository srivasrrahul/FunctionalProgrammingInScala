object Solution {
  def rob(nums: Array[Int]): Int = {
    nums.length match {
      case 0 => 0
      case 1 => nums(0)
      case _ => {
        val cache = new Array[Int](nums.length)
        cache(0) = nums(0)
        cache(1) = nums(1)
        for (j <- 2 to nums.length-1) {
          var maxRobbedValue = Int.MinValue
          for (k <- j-2 to 0 by -1) {
            val robbedValue = cache(k) + nums(j)
            if (robbedValue > maxRobbedValue) {
              maxRobbedValue = robbedValue
            }

            cache(j) = maxRobbedValue
          }
        }


        //println(cache.mkString(","))
        cache.max
      }
    }


  }

  def main(args: Array[String]): Unit = {
    println(rob(Array(2,7,9,3,1)))

  }
}