object Solution {
  def shiftingLetters(S: String, shifts: Array[Int]): String = {
    if (shifts.isEmpty) {
      S
    }else {
      val discreteShifts = new Array[Long](shifts.length)
      discreteShifts(discreteShifts.length - 1) = shifts.last
      for (j <- discreteShifts.length - 2 to 0 by -1) {
        discreteShifts(j) = discreteShifts(j + 1) + shifts(j)
      }

      //println(discreteShifts.mkString(","))
      val updatedStr = new StringBuilder
      for (j <- 0 to S.length - 1) {
        val currentChar = S(j) - 'a'
        //println(currentChar)
        val newChar = (currentChar + discreteShifts(j))%26
        //println(newChar)
        updatedStr.append(('a' + newChar).toChar)
      }
      updatedStr.toString()
    }
  }

  def main(args: Array[String]): Unit = {
    println(shiftingLetters("bad",Array(10,20,30)))
  }
}