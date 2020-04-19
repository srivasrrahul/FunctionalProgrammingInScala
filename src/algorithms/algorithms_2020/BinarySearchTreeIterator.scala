import scala.collection.mutable.ListBuffer

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

class BSTIterator(_root: TreeNode) {



  def getSortedLst(node : TreeNode) : List[TreeNode] = {
    if (node == null) {
      List[TreeNode]()
    }else {

      val leftLst = getSortedLst(node.left)
      val rightLst = getSortedLst(node.right)
      val retValue = new ListBuffer[TreeNode]

      retValue.addAll(leftLst)
      retValue.addOne(node)
      retValue.addAll(rightLst)

      retValue.toList
    }
  }
  var sortedLst : Option[List[TreeNode]] = None
  def init() : Unit = {
    val lst = getSortedLst(_root)
    sortedLst = Some(lst)

  }

  init()
  var current = sortedLst.get

  /** @return the next smallest number */
  def next(): Int = {
    val retValue = current.head.value
    current = current.tail
    retValue
  }

  /** @return whether we have a next smallest number */
  def hasNext(): Boolean = {
    current match {
      case (x::_) => true
      case _ => false
    }
  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val rootNode = new TreeNode(2)
    val left = new TreeNode(1)
    val right = new TreeNode(3)
    val rright = new TreeNode(10)

    rootNode.left = left
    rootNode.right = right


    right.right = rright

    val itr = new BSTIterator(rootNode)
    println(itr.hasNext())
    println(itr.next())
    println(itr.hasNext())
    println(itr.next())
    println(itr.hasNext())
    println(itr.next())
    println(itr.hasNext())
    println(itr.next())
  }
}
