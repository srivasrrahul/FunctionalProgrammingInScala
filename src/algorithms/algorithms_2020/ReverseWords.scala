import scala.collection.mutable.ListBuffer

object Solution {
  def reverseWords(s: String): String = {
    val tokenLst = new ListBuffer[String]
    val token = new StringBuilder
    for (ch <- s) {
      ch match {
        case ' ' => {
          val lastToken = token.toString()
          token.clear()
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
    tokenLst.reverse.mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    println(reverseWords("the sky is blue"))
  }
}