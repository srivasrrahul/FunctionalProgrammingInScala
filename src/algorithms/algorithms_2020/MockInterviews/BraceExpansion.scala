import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait Token
case class Value(val string: String) extends Token
case class ValueOption(val string: Array[String]) extends Token
object Solution {
  def expand(S: String): Array[String] = {
    def parse() : Array[Token] = {
      val arrayBuffer = new ArrayBuffer[Token]()
      var underBraces = false
      val myoptions = new ArrayBuffer[String]()

      for (j <- 0 to S.length-1) {
        S(j) match {
          case '{' => {
            underBraces = true
            myoptions.clear()

          }
          case '}' => {
            arrayBuffer.append(new ValueOption(myoptions.sortInPlace().toArray))
            myoptions.clear()
            underBraces = false
          }

          case letter if S(j).isLetter => {
            if (underBraces == true) {
              myoptions.append(letter.toString)
            }else {
              arrayBuffer.append(new Value(letter.toString))
            }

          }

          case _ => {

          }
        }
      }

      arrayBuffer.toArray
    }

    val parsedTokens = parse()

    val retValue = new ArrayBuffer[String]()
    def generateStr(j : Int,earlierStr : String) : Unit = {
      if (j == parsedTokens.length) {
        retValue.append(earlierStr)
      }else {
        parsedTokens(j) match {
          case Value(v) => {
            generateStr(j+1,earlierStr ++ v)
          }
          case ValueOption(strLst) => {
            for (str <- strLst) {
              generateStr(j+1,earlierStr ++ str)
            }
          }
        }
      }
    }

    generateStr(0,"")
    retValue.toArray
  }
}