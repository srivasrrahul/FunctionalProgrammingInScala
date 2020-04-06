import scala.collection.mutable
import util.control.Breaks._

object Solution {
  def isValid(s: String): Boolean = {
    val stack = new mutable.Stack[Char]()
    var retValue = true

    breakable {
      for (i <- 0 to s.length - 1) {
        s.charAt(i) match {
          case '(' | '[' | '{' => {
            stack.push(s.charAt(i))
          }
          case ')' | ']' | '}' => {
            if (stack.size == 0) {
              retValue = false
            } else {
              stack.pop() match {
                case '(' => {
                  if (s.charAt(i) != ')') {
                    retValue = false
                    break
                  }

                }
                case '[' => {
                  if (s.charAt(i) != ']') {
                    retValue = false
                    break
                  }
                }

                case '{' => {
                  if (s.charAt(i) != '}') {
                    retValue = false
                    break
                  }
                }
              }
            }
          }
        }
      }
    }

    if (retValue && stack.isEmpty == true) {
      true
    }else {
      false
    }
  }

  def main(args: Array[String]): Unit = {
    println(isValid("{}{}{}"))
  }
}