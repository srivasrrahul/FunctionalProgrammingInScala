import scala.collection.mutable.ListBuffer

object Solution {
  def reverseWords(s: Array[Char]): Unit = {
    val token = new StringBuilder
    val tokenLst = new ListBuffer[String]
    for (ch <- s) {
      ch match {
        case ' ' => {
          val lastToken = token.toString()
          token.clear
          if (lastToken.isEmpty == false) {
            tokenLst.append(lastToken)
          }
        }
        case _ => {
          token.append(ch)
        }
      }
    }

    if (token.isEmpty == false) {
      tokenLst.append(token.toString())
    }

    val retValue = tokenLst.reverse.toArray.mkString(" ").toArray

    //println(retValue.mkString(","))
    Array.copy(retValue,0,s,0,s.length)
    //println(s.mkString(","))
  }

  def main(args: Array[String]): Unit = {
    println(reverseWords(Array('t','h','e',' ','s','k','y',' ','i','s',' ','b','l','u','e')))
  }
}