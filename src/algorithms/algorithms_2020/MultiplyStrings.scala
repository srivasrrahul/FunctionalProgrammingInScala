object Solution {

  def addTwoString(x : String,y : String) : String = {
    if (x.length < y.length) {
      //println("test")
      addTwoString(y,x)
    }else {
      //println(x + " " + y)
      //x has highest count
      val res = new StringBuilder
      var carry = 0
      var k = x.length-1
      for (j <- y.length-1 to 0 by -1) {
        val localRes = x(k).asDigit + y(j).asDigit + carry
        res.append(localRes%10)
        if (localRes >= 10) {
          carry = (localRes - (localRes%10))/10
        }else {
          carry = 0
        }

        k = k - 1
      }

//      println(carry)
//      println(res.toString())

      val pending = x.length-y.length
      for (j <- pending-1 to 0 by -1) {

        val localRes = x(j).asDigit + carry
        res.append(localRes%10)
        if (localRes >= 10) {
          carry = (localRes - (localRes%10))/10
        }else {
          carry = 0
        }
      }

      if (carry > 0) {
        res.append(carry)
      }

      res.reverse.toString()
    }
  }
  def multiply(num1: String, num2: String): String = {
    if (num1.length < num2.length) {
      multiply(num2,num1)
    }else {
      if (num1 == "0" || num2 == "0") {
        "0"
      }else {
        var totalResult = ""


        for (j <- num2.length - 1 to 0 by -1) {
          val currentInt = num2(j).asDigit
          var carry = 0
          var result = 0
          var resultLst = new StringBuilder
          for (k <- num1.length - 1 to 0 by -1) {

            val currentTopInt = num1(k).asDigit

            val mul = (currentTopInt * currentInt) + carry
            //println(currentInt + " " + currentTopInt + " " + mul)

            resultLst.append(mul % 10)
            if (mul >= 10) {
              carry = (mul - (mul % 10)) / 10
            } else {
              carry = 0
            }
          }

          //println(result + " " + carry)
          //        val pow = Math.pow(10,num1.length).toInt
          //        result = result + (carry*pow)
          if (carry > 0) {
            resultLst.append(carry)
          }
          resultLst.reverseInPlace()
          //add zeros
          for (i <- 1 to num2.length - 1 - j) {
            resultLst.append("0")
          }
          //println(resultLst)

          totalResult = addTwoString(totalResult, resultLst.toString)

          //println(result)
          //        val nextPow = Math.pow(10,num2.length-1-j).toInt
          //        totalResult = totalResult + result*nextPow
        }

        totalResult.toString
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(multiply("612891238135","897787"))
    //println(addTwoString("127312367","989892"))
  }
}