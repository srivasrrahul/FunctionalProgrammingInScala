import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class MaxStack() {

  /** initialize your data structure here. */
  val pq = mutable.PriorityQueue.empty[Int]
  val stack = new mutable.Stack[Int]()

  def push(x: Int) : Unit = {
    pq.enqueue(x)
    stack.push(x)
  }

  def pop(): Int = {
    val poppedElement = stack.pop()

    val elementLst = new ListBuffer[Int]

    var topPQ = pq.dequeue()
    while (topPQ != poppedElement) {
      elementLst.append(topPQ)
      topPQ = pq.dequeue()
    }

    for (element <- elementLst) {
      pq.addOne(element)
    }

    poppedElement
  }

  def top(): Int = {
    stack.top
  }

  def peekMax(): Int = {
    pq.head
  }

  def popMax(): Int = {
    val maxElement = pq.dequeue()

    val lst = new ListBuffer[Int]

    var topElement = stack.pop()
    while (topElement != maxElement) {
      lst.addOne(topElement)
      topElement = stack.pop()
    }



    for (element <- lst.reverse) {
      stack.push(element)
    }

    maxElement


  }


}

object Solution {
  def main(args: Array[String]): Unit = {
    val pq = new mutable.PriorityQueue[Int]()(Ordering[Int].max)
    pq.addOne(5)
    pq.addOne(1)
    pq.addOne(6)
    println(pq)
    println(" + " + pq.head + " " + pq.dequeue())
    val stack = new MaxStack
    stack.push(5)
    stack.push(1)
    stack.push(6)
    println(stack.peekMax())
    println(stack.popMax())
    println(stack.popMax())
    println(stack.top)

//    stack.push(-2)
//    println(stack.popMax())
//    stack.push(-45)
//    stack.push(-82)
//    stack.push(29)
//    println(stack.pop)
//    println(stack.peekMax())

//    stack.push(1)
//    stack.push(2)
//    stack.push(7)
//    stack.push(3)
//    stack.push(4)
//    stack.push(10)
//    stack.push(9)
//    //stack.push(5)
//    //stack.push(3)
//    println(stack.popMax())
//    //println(stack.peekMax())
//    println(stack.popMax())
//    println(stack.popMax())
//    println(stack.pop())
//    println(stack.pop())
//    println(stack.pop())
//    println(stack.popMax())
//    println(stack.top())
    //println(stack.pop())

  }
}