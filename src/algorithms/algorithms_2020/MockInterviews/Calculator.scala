import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait Token
case object PlusToken extends Token
case object MinusToken extends Token
case object MulToken extends Token
case object DivToken extends Token
case class Numeric(val x : Int) extends Token
object Solution {
  def calculate(s: String): Int = {
    if (s.isEmpty) {
      0
    }else {
      def parse(): List[Token] = {
        var numericBuffer = new StringBuilder
        val tokens = new ListBuffer[Token]
        var negFound = false

        def fillNumeric(): Unit = {
          if (numericBuffer.isEmpty == false) {
            if (negFound == true) {
              tokens.append(new Numeric(numericBuffer.toString().toInt * (-1)))
            } else {
              tokens.append(new Numeric(numericBuffer.toString().toInt))
            }
            negFound = false
            numericBuffer.clear()
          }
        }


        for (j <- 0 to s.length - 1) {
          s(j) match {
            case '+' => {
              fillNumeric()
              tokens.append(PlusToken)

            }
            case '-' => {
              fillNumeric()
              negFound = true
              tokens.append(PlusToken)

            }
            case '*' => {
              fillNumeric()
              tokens.append(MulToken)

            }
            case '/' => {
              fillNumeric()
              tokens.append(DivToken)
              //fillNumeric()
            }
            case ' ' => {
              fillNumeric()
            }
            case dig if s(j).isDigit => {
              numericBuffer.append(s(j))
            }
            case _ => {
              //error
            }

          }


        }

        fillNumeric()
        tokens.toList
      }

      def isPriority(t1: Token, t2: Token): Int = {
        if (t1 == t2) {
          0
        } else {
          t1 match {
            case PlusToken => {
              t2 match {
                case MinusToken => {
                  1
                }
                case DivToken | MulToken => {
                  -1
                }
                case _ => {
                  println("erro")
                  0
                }
              }
            }
            case MinusToken => {
              t2 match {
                case PlusToken | DivToken | MulToken => {
                  -1
                }
                case _ => {
                  0
                }
              }
            }

            case MulToken => {
              t2 match {
                case PlusToken | MinusToken => {
                  1
                }
                case DivToken => {
                  1
                }
                case _ => {
                  1
                }

              }
            }
            case DivToken => {
              t2 match {
                case PlusToken | MinusToken => {
                  1
                }
                case MulToken => {
                  1
                }
                case _ => {
                  1
                }

              }
            }
            case _ => {
              0
            }
          }
        }
      }

      def convertToPostFix(lst: List[Token]): List[Token] = {
        val postfix = new ListBuffer[Token]
        val stack = new mutable.Stack[Token]()


        def extractFromStackTillTop(token: Token): Unit = {
          var found = false
          while (stack.isEmpty == false && found == false) {
            if (isPriority(stack.top, token) >= 0) {
              postfix.append(stack.pop)
            } else {
              found = true
            }
          }
        }

        for (l <- lst) {
          l match {
            case PlusToken | MinusToken | DivToken | MulToken => {
              extractFromStackTillTop(l)
              stack.push(l)
            }
            case _ => {
              postfix.append(l)
            }
          }
        }

        while (stack.isEmpty == false) {
          postfix.append(stack.pop())
        }

        postfix.toList
      }

      def evalPostfix(lst: List[Token]): Int = {
        val stack = new mutable.Stack[Int]()
        for (l <- lst) {
          l match {
            case Numeric(x) => {
              stack.push(x)
            }
            case _ => {
              val b = stack.pop()
              val a = stack.pop()
              l match {
                case PlusToken => {
                  stack.push(a + b)
                }
                case MinusToken => {
                  stack.push(a - b)
                }
                case MulToken => {
                  stack.push(a * b)
                }
                case _ => {
                  stack.push(a / b)
                }
              }
            }
          }
        }

        stack.pop()
      }

      val lst = parse()
      println(s)
      println(lst)
      val postFix = convertToPostFix(lst)
      println(postFix)
      evalPostfix(postFix)
    }
    //0
  }

  def main(args: Array[String]): Unit = {

    //println(calculate("2-3/4*2+4/5*6+7"))
    println(calculate(args(0)))
    //println(calculate("2-3/4*2+4/5*6+7"))
  }
}