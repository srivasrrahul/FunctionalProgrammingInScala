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

trait Expr
trait Term

case class NumbericTerm(val x : Int) extends Term
case class MulTerm(val x : Int,val y : Term) extends Term

case class AddExpr(val x : Term,val y : Expr) extends Expr
case class MinusExpr(val x : Term,val y : Expr) extends Expr

case class TermExpr(val x : Term) extends Expr

object Solution {

  def evalTerm(term : Term) : Int = {
    term match {
      case NumbericTerm(x) => {
        x
      }
      case MulTerm(x,y) => {
        x*evalTerm(y)
      }
    }
  }
  def evalExpr(expr: Expr) : Int = {
    expr match {
      case AddExpr(a,b) => {
        evalTerm(a) + evalExpr(b)
      }
      case MinusExpr(a,b) => {
        evalTerm(a) - evalExpr(b)
      }
      case TermExpr(term) => {
        evalTerm(term)
      }
    }
  }
  def parseTerm(tokens : Array[Token],currentIndex : Int) : (Term,Int) = {
    val first = tokens(currentIndex).asInstanceOf[Number].x
    if (currentIndex == tokens.length-1) {
      (new NumbericTerm(first),-1)
    }else {
      tokens(currentIndex+1) match {
        case MulOperator => {
          val (nextTerm,nextIndex) = parseTerm(tokens,currentIndex+2)
          val retValue = new MulTerm(first,nextTerm)
          (retValue,nextIndex)
        }
        case _ => {
          (new NumbericTerm(first),currentIndex+1)
        }
      }
    }
  }
  def parseExpr(tokens : Array[Token],currentIndex : Int) : (Expr,Int) = {
    //returns evaluated value and next token array index
    val (firstTerm,nextIndex) = parseTerm(tokens,currentIndex)
    if (nextIndex != -1) {
      tokens(nextIndex) match {
        case PlusOperator => {
          val (nextExpr,nextnextIndex) = parseExpr(tokens,nextIndex+1)
          (new AddExpr(firstTerm,nextExpr),nextnextIndex)
        }
        case _ => {
          val (nextExpr,nextnextIndex) = parseExpr(tokens,nextIndex+1)
          (new MinusExpr(firstTerm,nextExpr),nextnextIndex)
        }
      }
    }else {
      (new TermExpr(firstTerm),nextIndex)
    }

  }
  def tokenize(string: String) : Array[Token] = {
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


  def evalArr(arr : Array[Int],begin : Int,end : Int) : List[(Int,String)] = {
    val len = end-begin+1
    val solution = new ListBuffer[(Int,String)]()
    if (len == 1) {
      solution.append((arr(begin),arr(begin).toString))
    }else {
      for (j <- begin to end-1) {
        val leftMap = evalArr(arr,begin,j)
        val rightMap = evalArr(arr,j+1,end)
        for ((leftRes,leftStr) <- leftMap) {
          for ((rightRes,rightStr) <- rightMap) {
            //println(leftMap + " " + rightMap)
            solution.append(((leftRes+rightRes),(leftStr ++ "+" ++ rightStr)))
            solution.append(((leftRes-rightRes),(leftStr ++ "-" ++ rightStr)))
            solution.append(((leftRes*rightRes),(leftStr ++ "*" ++ rightStr)))
          }
        }

        //Now consider left and right and single digits
        //leftSubStr from begn to j
        if (j > begin) {
          //println("j")
          val leftArr = arr.slice(begin, j+1)
          val rightArr = arr.slice(j + 1, end+1)
          //println(leftArr.mkString(",") + " " + rightArr.mkString(","))

          val leftDig = leftArr.foldLeft(0)((acc, newVal) => {
            acc * 10 + newVal
          })

          val rightDig = rightArr.foldLeft(0)((acc, newVal) => {
            acc * 10 + newVal
          })


          solution.append(((leftDig + rightDig, leftDig.toString ++ "+" ++ rightDig.toString())))
          solution.append(((leftDig - rightDig, leftDig.toString ++ "-" ++ rightDig.toString())))
          solution.append(((leftDig * rightDig, leftDig.toString ++ "*" ++ rightDig.toString())))
        }
      }
    }

    solution.toList
  }
  def addOperators(num: String, target: Int): List[String] = {
    val arrBuffer = new scala.collection.mutable.ArrayBuffer[Int]()
    for (j <- 0 to num.length-1) {
      arrBuffer.append(num(j).asDigit)
    }

    val arr = arrBuffer.toArray
    val exprLsts = evalArr(arr,0,arr.length-1)

    val findlLsts = exprLsts.filter(x => x._1 == target)
    val solution = new mutable.HashSet[String]()
    for (findLst <- findlLsts) {
      if (solution.contains(findLst._2)) {
        val tokens = tokenize(findLst._2)
        val parsedTokens = parseExpr(tokens, 0)
        val evaluatedValue = evalExpr(parsedTokens._1)
        if (evaluatedValue == target) {
          solution.add(findLst._2)
        }
      }
    }
    println(solution.toList)

    solution.toList


  }

  def main(args: Array[String]): Unit = {
    println(addOperators("3456237490",9191))
  }
}