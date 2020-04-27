import scala.collection.mutable

class MinStack {

  /** initialize your data structure here. */
  val stack = new mutable.Stack[Int]()
  val minStack = new mutable.Stack[Int]()

  def push(x: Int) : Unit = {
    stack.push(x)
    if (minStack.isEmpty) {
      minStack.push(x)
    }else {
      if (x <= minStack.top) {
        minStack.push(x)
      }
    }
  }

  def pop() : Int = {
    val poppedValue = stack.pop()
    if (poppedValue == minStack.top) {
      minStack.pop
    }

    poppedValue
  }

  def top(): Int = {
    stack.top
  }

  def getMin(): Int = {
    minStack.top
  }

}