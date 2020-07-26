trait Reader4 {
  def read4(buf4: Array[Char]) : Int = {
    0
  }
}
class Solution extends Reader4 {
  /**
   * @param  buf Destination buffer
   * @param  n   Number of characters to read
   * @return     The number of actual characters read
   */
  var pendingArr : List[Char] = List() //This keeps past records
  var eofReached = false
  def readFromNetwork() : Unit = {
    val localArr = new Array[Char](4)
    val countRead =  read4(localArr)
    if (countRead < 4) {
      eofReached = true
    }

    pendingArr = localArr.toList
  }
  def read(buf: Array[Char], n: Int): Int = {
    if (eofReached) {
      0
    }else {

      var bufIndex = 0
      println(pendingArr)

      def copyPending() : Unit = {
        while (pendingArr.isEmpty == false && bufIndex < n) {
          buf(bufIndex) = pendingArr.head
          pendingArr = pendingArr.tail
          bufIndex = bufIndex + 1
        }
      }

      def readAndCopy() : Unit = {
        readFromNetwork()
        println(pendingArr)
        copyPending()
      }

      copyPending()

      if (bufIndex < n-1) {
        val len = n - bufIndex
        val n4Len = len / 4
        for (j <- 0 to n4Len if eofReached == false) {
          readAndCopy()
        }

        //for final ones
        if (eofReached == false && bufIndex < n) {
          readAndCopy()
        }
      }

      bufIndex
    }
  }
}