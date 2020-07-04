import scala.collection.mutable
import scala.collection.mutable.ListBuffer


case class ListParenthesis(val lstSubExpr : ListBuffer[ListParenthesis])

object Solution {


  def parseExpr(string: String) : ListParenthesis = {

    val stack = new mutable.Stack[Char]()
    def parse(currentIndex : Int,currentEnvelope : ListParenthesis) : Int = {
      string(currentIndex) match {
        case '(' => {
          val newEnvelop = new ListParenthesis(new ListBuffer[ListParenthesis])
          val stackCurrentPos = stack.size
          stack.push('(')
          var nextIndex = currentIndex
          while (stack.size != stackCurrentPos && nextIndex+1 < string.length) {
            nextIndex = parse(nextIndex+1,newEnvelop)
          }

          currentEnvelope.lstSubExpr.append(newEnvelop)

          nextIndex
        }
        case ')' => {
          //finish this
          stack.pop
          currentIndex
        }
      }
    }

    val topLst = new ListParenthesis(new ListBuffer[ListParenthesis])
    var index = -1
    while (index + 1 < string.length) {
      index = parse(index+1,topLst)
    }

    topLst

  }
  def eval(expr: ListParenthesis) : Int = {
    //println(expr)
    val subLst = expr.lstSubExpr
    if (subLst.size == 0) {
      1
    }else {
      var retValue = 0
      for (subExpr <- subLst) {
        retValue = retValue + eval(subExpr)
      }
      2*retValue
    }


  }

  def print(lst : ListParenthesis, tab : String) : Unit = {
    val subLsts = lst.lstSubExpr
    if (subLsts.size >= 1) {

      for (subLst <- subLsts) {
        println(tab + subLst)
        print(subLst,tab + " ")
      }
    }else {
      println(tab + subLsts)
    }
  }
  def scoreOfParentheses(S: String): Int = {
    val parsedValue = parseExpr(S)
    //println(parsedValue + " " + parsedValue.lstSubExpr.length)
    //print(parsedValue," ")
    var retValue = 0
    for (expr <- parsedValue.lstSubExpr) {
      //println(expr + " " + eval(expr))
      retValue = retValue + eval(expr)
    }
    //println(retValue)
//    println(" Final " + parsedValue)
    retValue

  }

  def main(args: Array[String]): Unit = {
    println("((())())")
    println(scoreOfParentheses("(())"))
  }
}