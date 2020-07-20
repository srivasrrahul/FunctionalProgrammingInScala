import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def evalPostFix(exprLst : List[String]) : Int = {
    val operandStack = new mutable.Stack[Int]()
    for (expr <- exprLst) {
      expr match {
        case "+" => {
          //println("plus")
          //println(operandStack)
          val y = operandStack.pop
          val x = operandStack.pop()
          operandStack.push(x+y)
        }
        case "-" => {
          val y = operandStack.pop
          val x = operandStack.pop()
          operandStack.push(x-y)
        }
        case "*" => {
          val y = operandStack.pop
          val x = operandStack.pop()
          operandStack.push(x*y)
        }
        case "/" => {
          //println(operandStack + " " + operandStack.top)
          val y = operandStack.pop
          val x = operandStack.pop()
          //println("Div " + x + " " + y)
          operandStack.push(x/y)
        }
        case _ => {
          operandStack.push(expr.toInt)
        }
      }
    }

    //println(operandStack)
    if (operandStack.isEmpty == false) {
      operandStack.pop
    }else {
      0
    }

  }
  def calculate(s: String): Int = {
    val token = new StringBuilder
    val postFix = new ListBuffer[String]
    val operatorStack = new mutable.Stack[Char]()
    // /*+-
    var factor = ""
    for (j <- 0 to s.length-1) {
      val ch = s(j)
      ch match {
        case ' ' => {
          if (token.isEmpty == false) {
            postFix.append(factor + token.toString())
            token.clear
            factor = ""

          }
        }
        case '/' => {
          if (token.isEmpty == false) {
            postFix.append(factor + token.toString())
            token.clear
            factor = ""

          }

          while (operatorStack.isEmpty == false && (operatorStack.head == '/' || operatorStack.head == '*')) {
            val top = operatorStack.pop().toString
            postFix.append(top)
          }

          operatorStack.push('/')
        }
        case '*' => {
          if (token.isEmpty == false) {
            postFix.append(factor + token.toString())
            token.clear
            factor = ""

          }

          while (operatorStack.isEmpty == false && (operatorStack.head == '*' || operatorStack.head == '/')) {
            val top = operatorStack.pop().toString
            postFix.append(top)
          }

          operatorStack.push('*')
        }
        case '+' => {
          if (token.isEmpty == false) {
            postFix.append(factor + token.toString())
            token.clear
            factor = ""

          }

          while (operatorStack.isEmpty == false && (operatorStack.head == '/' || operatorStack.head == '*')) {
            val top = operatorStack.pop().toString
            postFix.append(top)
          }

          operatorStack.push('+')
        }
        case '-' => {
          if (token.isEmpty == false) {
            postFix.append(factor + token.toString())
            token.clear
            factor = "-"

          }

          while (operatorStack.isEmpty == false && (operatorStack.head == '/' || operatorStack.head == '*' || operatorStack.head == '+')) {
            val top = operatorStack.pop().toString
            postFix.append(top)
          }

          operatorStack.push('+')
        }
        case _ => {
          token.append(ch)
        }
      }
    }

    if (token.isEmpty == false) {
      postFix.append(factor + token.toString())
    }

    while (operatorStack.isEmpty == false) {
      postFix.append(operatorStack.pop().toString)
    }

    val postFixExpr = postFix.toList
    //println(postFixExpr)
    //println(evalPostFix(postFixExpr))
    evalPostFix(postFixExpr)
  }

  def main(args: Array[String]): Unit = {
    println(calculate("1+2*5/3+6/4*2"))
  }
}