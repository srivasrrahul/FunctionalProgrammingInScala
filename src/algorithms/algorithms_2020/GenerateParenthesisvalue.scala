import scala.collection.mutable
import scala.collection.mutable.ListBuffer
trait Token
case class Number(val x : Int) extends Token
case object PlusOperator extends Token
case object MinusOperator extends Token
case object MulOperator extends Token
case object DivOperator extends Token
case object OpenBrace extends Token
case object ClosedBrace extends Token
object Solution {

  def parse(string: String) : Array[Token] = {
    val tokenLst = new scala.collection.mutable.ArrayBuffer[Token]
    val numberStr = new StringBuilder
    for (ch <- string) {
      ch match {
        case digit if ch.isDigit == true => {
          numberStr.append(digit)
        }
        case plusOperator if ch == '+' => {
          tokenLst.append(new Number(Integer.parseInt(numberStr.toString())))
          numberStr.clear
          tokenLst.append(PlusOperator)
        }
        case minusOperator if ch == '-' => {
          tokenLst.append(new Number(Integer.parseInt(numberStr.toString())))
          numberStr.clear
          tokenLst.append(MinusOperator)
        }
        case mulOperator if ch == '*' => {
          tokenLst.append(new Number(Integer.parseInt(numberStr.toString())))
          numberStr.clear
          tokenLst.append(MulOperator)
        }
        case divOperator if ch == '/' => {
          tokenLst.append(new Number(Integer.parseInt(numberStr.toString())))
          numberStr.clear
          tokenLst.append(DivOperator)
        }
      }
    }

    tokenLst.append(new Number(Integer.parseInt(numberStr.toString())))


    tokenLst.toArray
  }

  def evalOptions(tokens: Array[Token],begin : Int,end : Int,tab : String) : List[Int] = {

    //println(tab + " " + begin + " " + end)
    val len = end-begin+1
    if (len == 1) {
      List(tokens(begin).asInstanceOf[Number].x)
    }else {

      val retValue = new ListBuffer[Int]
      for (j <- begin to end - 2 by 2) {
        val leftLst = evalOptions(tokens, begin, j,tab + " ")
        val op = tokens(j + 1)
        val rightLst = evalOptions(tokens, j+2,end,tab + " ")
        for (first <- leftLst) {
          for (second <- rightLst) {
            op match {
              case PlusOperator => {
                retValue.append(first + second)
              }
              case MinusOperator => {
                retValue.append(first - second)
              }
              case MulOperator => {
                retValue.append(first * second)
              }
              case DivOperator => {
                retValue.append(first / second)
              }
            }
          }
        }
      }

      //println(tab + " " + retValue.toList)
      retValue.toList
    }

  }
  def diffWaysToCompute(input: String): List[Int] = {
    if (input.isEmpty) {
      List()
    }else {
      val tokenLst = parse(input)
      //println(tokenLst.mkString(","))
      val lst = evalOptions(tokenLst, 0, tokenLst.length - 1, "")
      //println(lst)
      lst
    }
  }

  def main(args: Array[String]): Unit = {
    println(diffWaysToCompute(""))
  }
}