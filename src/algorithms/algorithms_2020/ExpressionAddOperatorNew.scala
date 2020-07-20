import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solution {
  def evalPostFix(exprLst : List[String]) : Long = {
    val operandStack = new mutable.Stack[Long]()
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
          operandStack.push(expr.toLong)
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
  def calculate(s: String): Long = {
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

  def addOperators(num: String, target: Int): List[String] = {
    val cache = new mutable.HashMap[Int,List[String]]()
    def generate(index : Int) : List[String] = {
      //println("for index " + index)
      if (index == num.length) {
        List()
      }else {
        if (index == num.length - 1) {
          List(num.last.toString)
        } else {
          val id = index
          if (cache.contains(id)) {
            //println("cache hit")
            cache.get(id).get
          } else {
            val lstBuffer = new ListBuffer[String]
            for (j <- index+1 to num.length-1) {
              val number = num.substring(index, j)
              val nextLsts = generate(j)
              for (nextLst <- nextLsts) {
                lstBuffer.append(number + "+" + nextLst)
                lstBuffer.append(number + "-" + nextLst)
                lstBuffer.append(number + "*" + nextLst)
                //lstBuffer.append(number + "/" + nextLst)
              }
            }


            if (num(index) != '0') {
              lstBuffer.append(num.substring(index))
            }
            val retValue = lstBuffer.toList
            cache += ((id, retValue))
            retValue
          }
        }
      }
    }

    val exprLsts = generate(0)
    val retValue = new ListBuffer[String]
    for (expr <- exprLsts) {
      //println(expr)
      val calculatedValue = calculate(expr)
      if (calculatedValue == target) {
        retValue.append(expr)
      }
    }
    //println(exprLsts.mkString("\n"))
    retValue.toList
  }

  def main(args: Array[String]): Unit = {
    //println(calculate("3456237490"))
    println(addOperators("3456237490",9191))
  }
}