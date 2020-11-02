import scala.collection.mutable.ArrayBuffer

trait Token
case class Number(val num : Int) extends Token
case object PlusOperator extends Token
case object MinusOperator extends Token
case object MulOperator extends Token
case object DivOperator extends Token
case object BracketOpen extends Token
case object BracketClose extends Token

trait Expr
trait Term
trait Factor

case class PlainExpr(val term: Term) extends Expr
case class AddExpr(val term : Term,val expr : Expr) extends Expr
case class MinusExpr(val term : Term,val expr: Expr) extends Expr

case class PlainTerm(val factor : Factor) extends Term
case class MulTerm(val factor: Factor,val term : Term) extends Term
case class DivTerm(val factor: Factor,val term : Term) extends Term

case class NumFactor(val num : Int) extends Factor
case class BracketFactor(val expr : Expr) extends Factor

object Solution {
  def tokenize(string: String) : Array[Token] = {
    val tokens = new ArrayBuffer[Token]()
    for (ch <- string) {
      ch match {
        case '+' => tokens.append(PlusOperator)
        case '-' => tokens.append(MinusOperator)
        case '*' => tokens.append(MulOperator)
        case '/' => tokens.append(DivOperator)
        case '(' => tokens.append(BracketOpen)
        case ')' => tokens.append(BracketClose)
        case _ => tokens.append(new Number(ch.asDigit))
      }
    }

    tokens.toArray
  }

  def parseFactor(tokens : Array[Token],index : Int) : (Factor,Int) = {
    //println(tokens(index))
    tokens(index) match {
      case Number(x) => {
        (new NumFactor(x),index)
      }
      case _ => {
        //println("not here")
        //shud be bracket open
        val (exp,nexOffSet) = parseExpr(tokens,index+1)
        (new BracketFactor(exp),nexOffSet+1) //include ')' in parsing result
      }
    }
  }
  def parseTerm(tokens : Array[Token],index : Int) : (Term,Int) = {
    val (factor,nextOffSet) = parseFactor(tokens,index)
    if (nextOffSet != -1 && nextOffSet+1 < tokens.size) {
      tokens(nextOffSet+1) match {
        case MulOperator => {
          val (nextTerm,nextNextOffset) = parseTerm(tokens,nextOffSet+2)
          (new MulTerm(factor,nextTerm),nextNextOffset)
        }
        case DivOperator => {
          val (nextTerm,nextNextOffset) = parseTerm(tokens,nextOffSet+2)
          (new DivTerm(factor,nextTerm),nextNextOffset)
        }
        case PlusOperator | MinusOperator => {
          //println("here")
          (new PlainTerm(factor),nextOffSet)
        }
        case _ => {
          (new PlainTerm(factor),nextOffSet) //include last ')'
        }
      }

    }else {
      (new PlainTerm(factor),nextOffSet)
    }
  }
  def parseExpr(tokens : Array[Token],index : Int) : (Expr,Int) = {
    val (firstTerm,nextOffSet) = parseTerm(tokens,index)
    if (nextOffSet != -1 && nextOffSet+1 < tokens.size) {
      tokens(nextOffSet+1) match {
        case PlusOperator => {
          val (nextExpr,nextNextOffSet) = parseExpr(tokens,nextOffSet+2)
          (new AddExpr(firstTerm,nextExpr),nextNextOffSet)
        }
        case MinusOperator => {
          val (nextExpr,nextNextOffSet) = parseExpr(tokens,nextOffSet+1)
          (new MinusExpr(firstTerm,nextExpr),nextNextOffSet)
        }
        case BracketOpen | BracketClose => {
          (new PlainExpr(firstTerm),nextOffSet)
        }
      }

    }else {
      (new PlainExpr(firstTerm),nextOffSet)
    }
  }

  def evalFactor(factor: Factor) : Int = {
    factor match {
      case NumFactor(x) => x
      case BracketFactor(exp) => {
        evalExp(exp)
      }
    }
  }

  def evalTerm(term: Term) : Int = {
    term match {
      case PlainTerm(f) => {
        evalFactor(f)
      }
      case MulTerm(f,t) => {
        evalFactor(f) * evalTerm(t)
      }
      case DivTerm(f,t) => {
        evalFactor(f) / evalTerm(t)
      }
    }
  }

  def evalExp(expr: Expr) : Int = {
    expr match {
      case PlainExpr(t) => {
        evalTerm(t)
      }
      case AddExpr(t,e) => {
        evalTerm(t) + evalExp(e)
      }
      case MinusExpr(t,e) => {
        evalTerm(t) - evalExp(e)
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val str = "((2+3)*5)+6/2"
    println(str)
    val tokens = tokenize(str)
    println(tokens.toList)
    val expr = parseExpr(tokens,0)._1
    println(expr)
    println(evalExp(expr))
  }
}
