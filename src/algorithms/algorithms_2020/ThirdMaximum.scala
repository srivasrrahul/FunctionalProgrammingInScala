object Solution {
  def thirdMax(nums: Array[Int]): Int = {
    if (nums.length < 3) {
      nums.max
    } else {
      val max = nums.max
      var secondMax : Option[Int] = None
      for (j <- 0 to nums.length - 1) {
        secondMax match {
          case None => {
            if (nums(j) < max) {
              secondMax = Some(nums(j))
            }
          }
          case Some(x) => {
            if (nums(j) > x && nums(j) < max) {
              secondMax = Some(nums(j))
            }
          }
        }
      }

      secondMax match {
        case None => {
          max
        }
        case Some(secondMaxVal) => {
          var thirdMax : Option[Int] = None
          for (j <- 0 to nums.length-1) {
            thirdMax match {
              case None => {
                if (nums(j) < secondMaxVal) {
                  thirdMax = Some(nums(j))
                }
              }
              case Some(third) => {
                if (nums(j) > third && nums(j) < secondMaxVal) {
                  thirdMax = Some(nums(j))
                }
              }
            }
          }

          thirdMax match {
            case None => max
            case Some(y) => y
          }
        }
      }


    }
  }

  def main(args: Array[String]): Unit = {
    println(thirdMax(Array(2, 2,2,3, 1)))
  }

}