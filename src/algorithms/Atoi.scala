
import scala.collection.mutable.Queue
import util.control.Breaks._

trait ParsedState
case object SkippingInitial extends ParsedState
case object Parsing extends ParsedState

case class IntString(value : Queue[Int],positive : Boolean)

object Solution {
  def myAtoi(str: String): Int = {

    def parse(s1 : String) : Option[IntString] = {
      var positive = true
      var parsedState : ParsedState = SkippingInitial
      val queue = Queue[Int]()

      breakable {
        for (j <- 0 to s1.length - 1) {
          //println(s1.charAt(j))
          parsedState match {
            case SkippingInitial => {
              s1.charAt(j) match {
                case ' ' => {

                }
                case '+' => {
                  parsedState = Parsing
                }
                case '-' => {
                  parsedState = Parsing
                  positive = false
                }

                case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                  //A valid number
                  parsedState = Parsing
                  //queue.addOne(s1.charAt(j) - '0')
                  queue += s1.charAt(j) - '0'
                }
                case _ => {
                  break
                }
              }
            }
            case Parsing => {
              s1.charAt(j) match {
                case ' ' => {
                  break
                }
                case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                  //queue.addOne(s1.charAt(j) - '0')
                  queue += s1.charAt(j) - '0'
                }
                case _ => {
                  break
                }

              }
            }
          }
        }
      }

      parsedState match {
        case SkippingInitial => {
          None
        }
        case Parsing => {
          Some(IntString(queue, positive))
        }
      }
    }

    val parsedValue = parse(str)
    parsedValue match {
      case Some(intString) => {
        var q = intString.value
        var positive = intString.positive
        var parsedInt : Long = 0
        var outOfRange = false
        breakable {
          while (q.length > 0) {
            //println(parsedInt)
            val updatedVal: Long = parsedInt * 10 + q.dequeue
            if (updatedVal > Int.MaxValue) {
              outOfRange = true
              break

            }
            parsedInt = updatedVal
          }
        }


        if (outOfRange) {
          if (positive) {
            Int.MaxValue
          }else {
            Int.MinValue
          }
        }else {
          //println(parsedInt + " " + positive)
          if (positive == false) {
            val finalVal: Long = parsedInt * (-1)
            if (finalVal < Int.MinValue) {
              Int.MinValue
            } else {
              finalVal.toInt
            }
          } else {
            if (parsedInt > Int.MaxValue) {
              Int.MaxValue
            } else {
              parsedInt.toInt
            }
          }
        }


      }
      case None => {
        0
      }
    }

  }

  def main(args: Array[String]): Unit = {
     println(myAtoi("   9223372036854775808"))
  }
}