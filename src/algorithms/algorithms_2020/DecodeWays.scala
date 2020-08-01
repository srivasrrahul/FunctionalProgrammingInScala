import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def numDecodings(s: String): Int = {
    val validRange = Range(1,27)
    val validStrSet = new mutable.HashSet[String]()
    for (j <- 1 to 26) {
      validStrSet.add(j.toString)
    }
    val cache = new mutable.HashMap[Int,List[List[Int]]]()
    def itr(currentIndex : Int) : List[List[Int]] = {
      if (currentIndex >= s.length) {
        List()
      }else {

        if (cache.contains(currentIndex)) {
          cache.get(currentIndex).get
        }else {
          val retValue = new ListBuffer[List[Int]]
          val currentDigit = s(currentIndex).asDigit
          if (validStrSet.contains(currentDigit.toString)) {
            if (currentIndex + 1 < s.length) {
              val nextLsts = itr(currentIndex + 1)
              if (nextLsts.isEmpty == false) {
                for (nextLst <- nextLsts) {
                  retValue.append(s(currentIndex).asDigit :: nextLst)
                }
              }
            } else {
              retValue.append(List(s(currentIndex).asDigit))
            }
          }

          if (currentIndex + 1 < s.length) {
            val plusOne = s(currentIndex).asDigit * 10 + s(currentIndex + 1).asDigit
            if (validStrSet.contains(s.substring(currentIndex,currentIndex+2))) {
              if (currentIndex + 2 < s.length) {
                val nextLsts = itr(currentIndex + 2)
                if (nextLsts.isEmpty == false) {
                  for (nextLst <- nextLsts) {
                    retValue.append(plusOne :: nextLst)
                  }
                }
              } else {
                retValue.append(List(plusOne))
              }
            }
          }

          val finalLst = retValue.toList

//          if (s(currentIndex) == '0') {
//            println("In zero + " + finalLst.mkString(","))
//            finalLst
//          }

          cache += ((currentIndex,finalLst))
          finalLst
        }
      }
    }

    val res = itr(0)
    //println(res.mkString("\n"))
    res.size
  }

  def main(args: Array[String]): Unit = {
    //121233901023
    println(numDecodings("121233901023"))
  }
}