object Solution {
  def convertStringToInt(num : String) : Long = {
    var intVal : Long = 1L
    var totalValue : Long = 0L
    for (j <- num.length-1 to 0 by -1) {
      val current = num(j) - '0'
      var withPrecision = current * intVal + totalValue
      totalValue = withPrecision
      intVal = intVal * 10

    }

    totalValue
  }

  def convertIntString(longVal : Long) : String = {
    val stringBuilder = new StringBuilder

    var pending = longVal
    while (pending > 10) {
      val div = pending / 10
      stringBuilder.append(div)
      pending = pending % 10
    }

    stringBuilder.append(pending)
    stringBuilder.toString()
  }
  def addStrings(num1: String, num2: String): String = {
//    val firstInt : Long = convertStringToInt(num1).toLong
//    val secondInt : Long = convertStringToInt(num2).toLong
//
//    println(firstInt)
//    println(secondInt)
//
//    val result : Long = firstInt + secondInt
//    convertIntString(result)
    val result = new StringBuilder
    var carry = 0
    var currentIndex1 = num1.length-1
    var currentIndex2 = num2.length-1

    while (currentIndex1 >= 0 && currentIndex2 >= 0) {
      val digitSum = (num1(currentIndex1) - '0') + (num2(currentIndex2) - '0') + carry
      println(digitSum)
      if (digitSum > 9) {
        val updatedDigitSum = digitSum % 10
        carry = digitSum / 10
        result.append(updatedDigitSum)
      }else {
        carry = 0
        //println((digitSum + '0'))
        result.append(digitSum)
      }

      currentIndex1 -= 1
      currentIndex2 -= 1

    }

    //println("Pending " + currentIndex1)
    if (currentIndex1 >= 0) {
      while (currentIndex1 >= 0) {
        val digitSum = (num1(currentIndex1) - '0') + carry
        //println("DigitSum is " + digitSum)
        if (digitSum > 9) {
          val updatedDigitSum = digitSum % 10
          carry = digitSum / 10
          result.append(updatedDigitSum)
        }else {
          carry = 0
          result.append(digitSum)
        }

        currentIndex1 -= 1

      }
    }



    if (currentIndex2 >= 0) {
      while (currentIndex2 >= 0) {
        val digitSum = (num2(currentIndex2) - '0') + carry
        if (digitSum > 9) {
          val updatedDigitSum = digitSum % 10
          carry = digitSum / 10
          result.append(updatedDigitSum)
        }else {
          carry = 0
          result.append(digitSum)
        }

        currentIndex2 -= 1
      }
    }

    if (carry > 0) {
      result.append(carry)
    }

    result.reverse.toString()

  }

  def main(args: Array[String]): Unit = {
    //println(convertStringToInt("100"))
    //println(convertIntString(100))
    println(addStrings("99929","99"))
  }
}