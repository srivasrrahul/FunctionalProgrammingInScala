import scala.collection.mutable.{ArrayBuffer, ListBuffer}

trait Token
case class Number(val x : Int) extends Token
case object Plus extends Token
case object Minus extends Token
case object Multiply extends Token
case object Div extends Token


trait Expr
trait Term

case class TermNumber(val x : Int) extends Term
case class TermMul(val x : Int,val y : Term) extends Term
case class TermDiv(val x : Int,val y : Term) extends Term

case class AddExpr(val x : Term,val y : Expr) extends Expr
case class MinusExpr(val x : Term,val y : Expr) extends Expr
case class TermExpr(val x : Term) extends Expr


object Solution {
  def calculate(s: String): Int = {
    def evalTerm(term : Term) : Int = {
      term match {
        case TermNumber(x) => x
        case TermMul(x,y) => evalTerm(y) * x
        case TermDiv(x,y) => evalTerm(y) / x
      }
    }
    def evalExpr(expr : Expr) : Int = {
      expr match {
        case AddExpr(term1,expr1) => evalTerm(term1) + evalExpr(expr1)
        case MinusExpr(term1,expr1) => evalTerm(term1) - evalExpr(expr1)
        case TermExpr(term1) => evalTerm(term1)
      }
    }
    def parseTerm(tokens: Array[Token],currentIndex : Int) : (Term,Int) = {
      //till what place * or div exists

      val headTerm = new TermNumber(tokens(currentIndex).asInstanceOf[Number].x)
      val prevTerm = new ListBuffer[Term] //only one element list

      prevTerm.append(headTerm)

      var parsedTill = currentIndex
      if (currentIndex + 1 < tokens.length) {
        var termPresent = true
        for (j <- currentIndex+1 to tokens.length-1 by 2 if termPresent == true) {
          tokens(j) match {
            case Multiply => {
              val oldTerm = prevTerm.head
              prevTerm.clear()
              val newTerm = new TermMul(tokens(j+1).asInstanceOf[Number].x,oldTerm)
              prevTerm.append(newTerm)
              parsedTill = j+1
            }
            case Div => {
              val oldTerm = prevTerm.head
              prevTerm.clear()
              val newTerm = new TermDiv(tokens(j+1).asInstanceOf[Number].x,oldTerm)
              prevTerm.append(newTerm)
              parsedTill = j+1
            }
            case _ => {
              termPresent = false
            }
          }
        }


      }

      (prevTerm.head,parsedTill)
    }

    def parseExpr(tokens : Array[Token],currentIndex : Int) : (Expr,Int) = {
      val (parsedTerm,parsedTill1) = parseTerm(tokens,currentIndex)
      //println(parsedTill1)
      if (parsedTill1 + 1 < tokens.length) {
        tokens(parsedTill1+1) match {
          case Plus => {
            val (parsedNextExpr,parsedTill2) = parseExpr(tokens,parsedTill1+2)
            val retExpr = new AddExpr(parsedTerm,parsedNextExpr)
            (retExpr,parsedTill2)
          }
          case _ => {
            val (parsedNextExpr,parsedTill2) = parseExpr(tokens,parsedTill1+2)
            val retExpr = new MinusExpr(parsedTerm,parsedNextExpr)
            (retExpr,parsedTill2)
          }
        }
      }else {
        val retExpr = new TermExpr(parsedTerm)
        (retExpr,parsedTill1)
      }
    }

    def tokenize() : Array[Token] = {
      val accumulator = new StringBuilder
      val tokens = new ArrayBuffer[Token]
      var ifMinus = false
      def mulFactor() : Int = {
        if (ifMinus == true) {
          ifMinus = false
          -1
        }else {
          1
        }
      }
      for (j <- 0 to s.length-1) {
        s(j) match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
            accumulator.append(s(j))
          }
          case '-' => {
            if (accumulator.size > 0) {
              tokens.append(new Number(accumulator.toString().toInt*mulFactor()))
              accumulator.clear
            }

            tokens.append(Plus)
            ifMinus = true

          }
          case '+' => {
            if (accumulator.size > 0) {
              tokens.append(new Number(accumulator.toString().toInt*mulFactor()))
              accumulator.clear
            }

            tokens.append(Plus)
          }
          case '*' => {
            if (accumulator.size > 0) {
              tokens.append(new Number(accumulator.toString().toInt*mulFactor()))
              accumulator.clear
            }

            tokens.append(Multiply)
          }

          case '/' => {
            if (accumulator.size > 0) {
              tokens.append(new Number(accumulator.toString().toInt*mulFactor()))
              accumulator.clear
            }

            tokens.append(Div)
          }
          case ' ' => {
            if (accumulator.size > 0) {
              tokens.append(new Number(accumulator.toString().toInt*mulFactor()))
              accumulator.clear
            }
          }
        }
      }

      if (accumulator.size > 0) {
        tokens.append(new Number(accumulator.toString().toInt*mulFactor()))
        accumulator.clear
      }

      tokens.toArray

    }

    val tokens = tokenize()
    println(tokens.mkString(","))
    val parseTree = parseExpr(tokens,0)
    println(parseTree._1)
    val retValue = evalExpr(parseTree._1)
    //println(retValue)


    retValue
  }

  def main(args: Array[String]): Unit = {
    println(calculate("3+2*2"))
  }
}