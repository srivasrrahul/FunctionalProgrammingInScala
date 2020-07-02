import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def findAndReplacePattern(words: Array[String], pattern: String): List[String] = {
    def ifMatch(word : String) : Boolean = {
      val matcher = new mutable.HashMap[Char,Char]()
      val taken = new mutable.HashSet[Char]() //pattern is taken
      var matchFound = true
      for (j <- 0 to word.length-1 if matchFound == true) {
        val source = word(j)
        val dest = pattern(j)
        matcher.get(source) match {
          case None => {
            if (taken.contains(dest)) {
              //dest is already mapped to somebody
              matchFound = false
            }else {
              matcher += ((source,dest))
              taken.add(dest)
            }
          }
          case Some(p) => {
            if (p != dest) {
              matchFound = false
            }
          }
        }
      }

      matchFound
    }

    val lstBuffer = new ListBuffer[String]
    for (word <- words) {
      if (ifMatch(word)) {
        lstBuffer.append(word)
      }
    }

    lstBuffer.toList
  }

  def main(args: Array[String]): Unit = {
    println(findAndReplacePattern(Array("abc","deq","mee","aqq","dkd","ccc"),"abb"))
  }
}