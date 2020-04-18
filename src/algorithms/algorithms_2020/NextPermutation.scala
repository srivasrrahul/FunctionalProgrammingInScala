
import util.control.Breaks._
object Solution {
  def nextPermutation(nums: Array[Int]): Unit = {
    var j = nums.length-1
    breakable {
      while (true) {
        if (j == 0) {
          //println("First")
          val l = nums.length
          for (j <- 0 to (l/2)-1) {
            val t = nums(j)
            nums(j) = nums(l-j-1)
            nums(l-j-1) = t
          }
          break()
        }else {
          //println(j)
          val k = j-1
          if (nums(k) < nums(j)) {
            var minVal : Option[(Int,Int)] = None

            for (i <- k+1 to nums.length-1) {
              minVal match {
                case Some((x,_)) => {
                  if (nums(i) < x && nums(i) > nums(k)) {
                    minVal = Some((nums(i),i))
                  }
                }
                case None => {
                  minVal = Some((nums(i),i))
                }
              }

            }

            var temp = nums(k)
            nums(k) = nums(minVal.get._2)
            nums(minVal.get._2) = temp

            java.util.Arrays.sort(nums,k+1,nums.length)


            break
          }

          j = j - 1
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
//    val arr  = Array(1,2,3,4)
//    nextPermutation(arr)
//    nextPermutation(arr)
//    nextPermutation(arr)
//    nextPermutation(arr)
//    nextPermutation(arr)
//    nextPermutation(arr)
//    nextPermutation(arr)
//    nextPermutation(arr)

    val arr  = Array(4,3,2,1)
    nextPermutation(arr)
    println(arr.mkString(","))
  }
}