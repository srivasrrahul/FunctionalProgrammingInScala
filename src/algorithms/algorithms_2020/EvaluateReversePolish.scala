import scala.collection.mutable

object Solution {
  def evalRPN(tokens: Array[String]): Int = {
    val stack = new mutable.Stack[String]()
    for (token <- tokens) {
      //println(token)
      token match {
        case "+" => {
          val b = stack.pop().toInt
          val a = stack.pop().toInt
          stack.push((a+b).toString)
        }
        case "-" => {
          val b = stack.pop().toInt
          val a = stack.pop().toInt
          stack.push((a-b).toString)
        }
        case "/" => {
          val b = stack.pop().toInt
          val a = stack.pop().toInt
          stack.push((a/b).toString)
        }
        case "*" => {
          val b = stack.pop().toInt
          val a = stack.pop().toInt
          stack.push((a*b).toString)
        }
        case _ => {
          stack.push(token)
        }
      }
    }

    if (stack.isEmpty == false) {
      stack.pop().toInt
    }else {
      0
    }

  }

  def main(args: Array[String]): Unit = {
    println(evalRPN(Array()))
  }
}