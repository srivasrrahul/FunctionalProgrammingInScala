object Solution {

  def productExceptSelf(nums: Array[Int]): Array[Int] = {
    var allProduct : Int = 1
    val result = new Array[Int](nums.length)
    var zeroCount = 0
    for (j <- 0 to nums.length-1) {
      allProduct = allProduct * nums(j)
      if (nums(j) == 0) {
        zeroCount += 1
      }
    }

    if (zeroCount <= 1) {
      val prefixMultiply = new Array[Int](nums.length)
      val suffixMultiply = new Array[Int](nums.length)

      prefixMultiply(0) = 1
      suffixMultiply(nums.length-1) = 1

      for (j <- 1 to prefixMultiply.length-1) {
        prefixMultiply(j) = prefixMultiply(j-1) * nums(j-1)
      }

      for (j <- suffixMultiply.length-2 to 0 by -1) {
        suffixMultiply(j) = suffixMultiply(j+1) * nums(j+1)
      }



      //println(prefixMultiply.mkString(","))
      //println(suffixMultiply.mkString(","))
      val Len = result.length-1
      for (j <- 0 to result.length-1) {
        j match {
          case 0 => {
            result(j) = suffixMultiply(j)
          }
          case Len  => {
            result(j) = prefixMultiply(j)
          }
          case _ => {
            result(j) = prefixMultiply(j) * suffixMultiply(j)
          }
        }

      }
    }

    result





  }


  def main(args: Array[String]): Unit = {
    println(productExceptSelf(Array(1,2,3,4)).mkString(","))
  }
}