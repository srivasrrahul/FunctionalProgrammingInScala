import scala.collection.mutable
import scala.util.control.Breaks._

object Solution {
  def validateStackSequences(pushed: Array[Int], popped: Array[Int]): Boolean = {
    val stack = new mutable.Stack[Int]()

    var j = 0
    var k = 0

    while (j < pushed.length && k < popped.length) {
      if (pushed(j) == popped(k)) {
        stack.push(pushed(j))
        stack.pop()

        j = j + 1
        k = k + 1
      }else {

        if (stack.isEmpty == false && stack.top == popped(k)) {
          while (stack.isEmpty == false && stack.top == popped(k)) {
            k = k + 1
            stack.pop()
          }
        }else {
          while (j < pushed.length && pushed(j) != popped(k)) {
            stack.push(pushed(j))
            j = j + 1
          }

          if (j < pushed.length && pushed(j) == popped(k)) {
            stack.push(pushed(j))
            stack.pop()
            j = j + 1
            k = k + 1
          }
        }
      }
    }

    //println(" End lop " + j + " " + k + " " + stack + " ")
    if (j == pushed.length && k == popped.length) {
      true
    }else {
      if (k < popped.length && stack.isEmpty) {
        false
      }else {
        breakable {
          while (k < popped.length && stack.top == popped(k)) {
            //println(" here ")
            stack.pop
            k = k + 1
            if (stack.isEmpty) {
              break
            }
          }
        }

        //println(" " + stack.size + " " + popped.length)
        if (stack.isEmpty == false || k < popped.length) {
          false
        }else {
          true
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(validateStackSequences(Array(2,1,3,0),Array(1,0,3,2)))
  }
}