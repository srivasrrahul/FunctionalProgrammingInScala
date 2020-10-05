import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Token
case object PlusToken extends Token
case object MinusToken extends Token
case object BraceOpenToken extends Token
case object BraceClosedToken extends Token

case class Value(val x : Int) extends Token
object Solution {
  def eval(expr : List[Token]) : Int = {
    val evalStack = new mutable.Stack[Int]()
    for (token <- expr) {
      token match {
        case Value(v) => {
          evalStack.push(v)
        }
        case PlusToken => {
          val top1 = evalStack.pop()
          val top2 = evalStack.pop()
          evalStack.push(top2+top1)
        }
        case MinusToken => {
          val top1 = evalStack.pop()
          val top2 = evalStack.pop()
          evalStack.push(top2-top1)
        }
      }
    }

    evalStack.pop()
  }
  def calculate(s: String): Int = {
    def parseToken(string: String) : Token = {
      string match {
        case "+" => PlusToken
        case "-" => MinusToken
        case "(" => BraceOpenToken
        case ")" => BraceClosedToken
        case _ => new Value(string.toInt)
      }
    }
    def tokenize() : List[Token] = {
      val lstBuffer = new ListBuffer[Token]
      val lastToken = new StringBuilder
      for (j <- 0 to s.length-1) {
        s(j) match {
          case ' ' => {
            if (lastToken.isEmpty == false) {
              lstBuffer.append(parseToken(lastToken.toString()))
              lastToken.clear()
            }
          }
          case '+' => {
            if (lastToken.isEmpty == false) {
              lstBuffer.append(parseToken(lastToken.toString()))
              lastToken.clear()
            }
            lstBuffer.append(PlusToken)
          }
          case  '-' => {
            if (lastToken.isEmpty == false) {
              lstBuffer.append(parseToken(lastToken.toString()))
              lastToken.clear()
            }
            lstBuffer.append(MinusToken)
          }
          case '(' => {
            if (lastToken.isEmpty == false) {
              lstBuffer.append(parseToken(lastToken.toString()))
              lastToken.clear()
            }
            lstBuffer.append(BraceOpenToken)
          }
          case ')' => {
            if (lastToken.isEmpty == false) {
              lstBuffer.append(parseToken(lastToken.toString()))
              lastToken.clear()
            }
            lstBuffer.append(BraceClosedToken)
          }
          case _ => {
            lastToken.append(s(j))
          }
        }
      }

      if (lastToken.isEmpty == false) {
        lstBuffer.append(parseToken(lastToken.toString()))
        lastToken.clear()
      }

      lstBuffer.toList
    }

    val tokenLst = tokenize()
    //println(tokenLst)
    val operantorStack = new mutable.Stack[Token]()

    val expr = new ListBuffer[Token]
    for (token <- tokenLst) {
      token match {
        case Value(v) => {
          expr.append(new Value(v))
        }
        case PlusToken => {
          //its the hightest operator

          if (operantorStack.isEmpty == false && (operantorStack.top == MinusToken || operantorStack.top == PlusToken) ) {
            while (operantorStack.isEmpty == false && (operantorStack.top == MinusToken || operantorStack.top == PlusToken)) {
              expr.append(operantorStack.pop)
            }
          }

          operantorStack.push(PlusToken)
        }
        case BraceOpenToken => {
          operantorStack.push(BraceOpenToken)
        }
        case MinusToken => {
          if (operantorStack.isEmpty == false && (operantorStack.top == MinusToken || operantorStack.top == PlusToken) ) {
            while (operantorStack.isEmpty == false && (operantorStack.top == MinusToken || operantorStack.top == PlusToken) ) {
              expr.append(operantorStack.pop)
            }
          }

          operantorStack.push(MinusToken)
        }
        case BraceClosedToken => {
          //println(operantorStack)
          while (operantorStack.top != BraceOpenToken) {
            expr.append(operantorStack.pop)
          }

          operantorStack.pop
        }
      }
    }

    while (operantorStack.isEmpty == false) {
      expr.append(operantorStack.pop)
    }

    //println(expr)
    val retValue = eval(expr.toList)
    retValue
  }

  def main(args: Array[String]): Unit = {
    println(calculate("10+(21+ 3-4)"))
  }
}