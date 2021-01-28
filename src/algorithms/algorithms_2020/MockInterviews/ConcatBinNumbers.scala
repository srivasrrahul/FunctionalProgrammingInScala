object Solution {
  def concatenatedBinary(n: Int): Int = {
    def itr(pastValue : Int,pastLen : Int,j : Int) : Int = {
      if (j > n) {
        pastValue
      }else {
        val jBinaryStr = j.toBinaryString
        val nextValue = ((math.pow(2,jBinaryStr.length).toLong * pastValue) % (math.pow(10,9).toInt+7) + j).toInt
        itr(nextValue,pastLen + jBinaryStr.length,j+1)
      }
    }

    itr(0,0,1)
  }
}