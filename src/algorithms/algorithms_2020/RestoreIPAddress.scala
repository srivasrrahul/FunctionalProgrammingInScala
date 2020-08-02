import scala.collection.mutable.ListBuffer

object Solution {
  def validString(string: String) : Boolean = {
    if (string.length <= 3 && string.length>0 && string.toInt >= 0 && string.toInt <= 255) {
      if (string(0) == '0') {
        string.length == 1
      }else {
        true
      }
    }else {
      false
    }
  }
  def restoreIpAddresses(s: String): List[String] = {
    val ipAddresses = new ListBuffer[String]
    def itr(currentIndex : Int,pastSavePointIndex : Int,ipAddress : List[String]) : Unit = {
      if (currentIndex >= s.length) {
        val subString = s.substring(pastSavePointIndex)
        if (validString(subString) && ipAddress.length==3) {
          ipAddresses.append((subString::ipAddress).reverse.mkString("."))
        }
      }else {
        if (ipAddress.length < 4) {
          val diff = currentIndex-pastSavePointIndex+1
          val subString = s.substring(pastSavePointIndex,currentIndex+1)
          if (validString(subString)) {
            itr(currentIndex+1,currentIndex+1,subString::ipAddress)
          }

          if (diff <= 3) {
            itr(currentIndex+1,pastSavePointIndex,ipAddress)
          }

        }
      }
    }

    itr(0,0,List())
    ipAddresses.toList
  }

  def main(args: Array[String]): Unit = {
    println(restoreIpAddresses("010010"))
  }
}