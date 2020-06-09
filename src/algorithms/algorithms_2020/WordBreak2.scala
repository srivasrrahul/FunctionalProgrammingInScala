import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def wordBreak(s: String, wordDict: List[String]): List[String] = {
    val wordSet = wordDict.toSet
    val matrix = Array.ofDim[String](s.length,s.length)

    for (j <- 0 to matrix.length-1) {
      for (k <- 0 to matrix(j).length-1) {
        matrix(j)(k) = ""
      }
    }

    //println("here")
    //var endFound = false
    for (j <- 0 to matrix.length-1) {
      for (k <- j to matrix(j).length-1 ) {
        val subString = s.substring(j,k+1)
        if (wordSet.contains(subString)) {
          //println(subString)
          matrix(j)(k) = subString

        }
      }
    }

    val cache = new mutable.HashMap[Int,List[List[String]]]()
    def itr(currentRow : Int) : List[List[String]] = {
      //println("Current row is " + currentRow)
      if (currentRow >= matrix.length) {
        List()
      }else {
        if (currentRow == matrix.length-1) {

        }
        if (cache.contains(currentRow)) {
          //println("cache hit")
          cache.get(currentRow).get
        } else {
          val lstBuffer = new ListBuffer[List[String]]
          for (k <- 0 to matrix(currentRow).length - 1) {
            val str = matrix(currentRow)(k)

            if (str != "") {
              //println("Str is " + str)
              val pendingLsts = itr(k + 1)
              if (pendingLsts.isEmpty == false) {
                for (pendingLst <- pendingLsts) {
                  lstBuffer.append((str :: pendingLst))
                }
              } else {
                if (k == s.length-1) {
                  lstBuffer.append(List(str))
                }
              }


            }
          }

          val findlLst = lstBuffer.toList
          cache += ((currentRow,findlLst))
          findlLst
        }
      }
    }


    val results = itr(0)
    val retValue = new ListBuffer[String]
    for (res <- results) {
      val totalLength = res.foldLeft(0)((acc,newVal) => acc + newVal.length)
      if (totalLength == s.length) {
        retValue.append(res.mkString(" "))
      }

    }
    //println(retValue)
    retValue.toList


  }

  def main(args: Array[String]): Unit = {
    //println(wordBreak("catsanddog",List("cat", "cats", "and", "sand", "dog")))
    //println(wordBreak("a",List("a")))
    val str = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    val lst = List("a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa")

    println(wordBreak(str,lst))
  }
}