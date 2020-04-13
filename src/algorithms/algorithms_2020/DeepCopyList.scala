import scala.collection.mutable

class Node(var _value: Int) {
    var value: Int = _value
    var next: Node = null
    var random: Node = null
 }


object Solution {
  def copyRandomList(head: Node): Node = {
    var first = head
    var newFirst : Node = null
    var newCurrent : Node = null
    val index = new mutable.HashMap[Int,(Node,Node)]()
    val nodeIndex = new mutable.HashMap[Node,Int]()
    var count = 0
    while (first != null) {
      var presentItr : Node = null
      if (newFirst == null) {
        newFirst = new Node(first.value)
        newCurrent = newFirst
        presentItr = newCurrent
      }else {
        newCurrent.next = new Node(first.value)
        presentItr = newCurrent
        newCurrent = newCurrent.next
      }



      index += ((count,(first,newCurrent)))
      nodeIndex += ((first,count))
      first = first.next
      count += 1
    }

    println(nodeIndex)
    index.foreachEntry((pos,node) => {
      val randomNextInOld = node._1.random
      if (randomNextInOld != null) {
        val indexForOld = nodeIndex.get(randomNextInOld).get
        val newNodeAtSameIndex = index.get(indexForOld).get._2

        node._2.random = newNodeAtSameIndex
      }
    })

    newFirst
  }

  def testCase1() : Unit = {
    val first = new Node(7)
    val second = new Node(13)
    val third = new Node(11)
    val fourth = new Node(10)
    val fifth = new Node(1)
    first.next = second
    second.next = third
    third.next = fourth
    fourth.next = fifth
    first.random = null
    second.random = first
    third.random = fifth
    fourth.random = third
    fifth.random = first

    val randomHead = copyRandomList(first)
    println(randomHead.next.random.value)
    println(first.next.random.value)
  }

  def testCase2() : Unit = {
    val first = new Node(1)
    val second = new Node(2)
    first.next = second
    first.random = second
    second.random = second

    val randomHead = copyRandomList(first)
    println(randomHead.next.random.value)
    println(first.next.random.value)


  }

  def main(args: Array[String]): Unit = {
    testCase2()
  }
}
