
object Solution {
  def maxProduct(nums: Array[Int]): Int = {
    //println("input is " + nums.mkString(","))
    val productArr = new Array[Int](nums.length)
    productArr(0) = nums(0)
    for (j <- 1 to productArr.length-1) {
      productArr(j) = productArr(j-1)*nums(j)
    }

    def getProduct(x : Int,y : Int) : Int = {
      //println("x " + x + " y " + y)
      if (x > y) {
        //Invalid return default
        1
      }else {
        val total = productArr(y)
        var priorValue = productArr(0)
        if (x > 0) {
          priorValue = productArr(x - 1)
        }

        if (priorValue == 0) {
          var res = 1
          for (j <- x to y) {
            res = res * nums(j)
          }

          res
        } else {
          //println("Returning " + (total / priorValue))
          total / priorValue
        }
      }
    }

    val maxProductArr = new Array[Int](nums.length)
    def maxProductItr(currentIndex : Int) : Option[Int] = { //last index + if it has encountered negative
      if (currentIndex == 0) {
        maxProductArr(0) = nums(currentIndex)
        if (nums(0) >= 0) {
          None
        }else {
          Some(0)
        }
      }else {

        val retValue = maxProductItr(currentIndex-1)
        //println("currentIndex " + currentIndex)
        retValue match {
          case None => {
            //No negative prior
            maxProductArr(currentIndex) = scala.math.max(nums(currentIndex),nums(currentIndex)*maxProductArr(currentIndex-1))
            if (nums(currentIndex) < 0) {
              Some(currentIndex)
            }else {
              None
            }
          }
          case Some(earlierNegativeIndex) => {
            val earlierNegativeValue = nums(earlierNegativeIndex)
            val interimProduct = getProduct(earlierNegativeIndex+1,currentIndex-1)
            var priorProductBeforeNegative = 1
            if (earlierNegativeIndex > 0) {
              priorProductBeforeNegative = maxProductArr(earlierNegativeIndex-1)
            }

            val option1 = nums(currentIndex)*interimProduct*nums(earlierNegativeIndex)
            val option2 = nums(currentIndex)*interimProduct*nums(earlierNegativeIndex) * priorProductBeforeNegative
            val option3 = nums(currentIndex)
            val option4 = nums(currentIndex) * maxProductArr(currentIndex-1)
            maxProductArr(currentIndex) = Array(option1,option2,option3,option4).max

            if (nums(currentIndex) < 0) {
              Some(currentIndex)
            }else {
              Some(earlierNegativeIndex)
            }
          }
        }


      }
    }

    maxProductItr(nums.length-1)
    //println(maxProductArr.mkString(","))
    maxProductArr.max
  }

  def main(args: Array[String]): Unit = {
    println(maxProduct(Array(2,-5,-2,-4,3)))
  }
}