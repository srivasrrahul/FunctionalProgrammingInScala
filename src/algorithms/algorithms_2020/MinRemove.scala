import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Index()
object Solution {
  def minRemoveToMakeValid(s: String): String = {
    val stack = new mutable.Stack[Int]()

    val removed = new mutable.HashSet[Int]()
    for (j <- 0 to s.length-1) {
      s(j) match {
        case '(' => stack.push(j)
        case ')' => {
          if (stack.size == 0) {
            //invalid char
            removed.add(j)
          }else {
            stack.pop()
          }
        }
        case _ => {

        }
      }
    }

    if (stack.length > 0) {
      while (stack.isEmpty == false) {
        removed.add(stack.pop())
      }
    }

    val stringBuilder = new StringBuilder
    for (j <- 0 to s.length-1) {
      if (removed.contains(j) == false) {
        stringBuilder.append(s(j))
      }
    }

    stringBuilder.toString
  }

  def main(args: Array[String]): Unit = {
    println(minRemoveToMakeValid(""))
  }
}