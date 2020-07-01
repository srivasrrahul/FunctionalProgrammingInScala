import scala.collection.mutable

class Node(var _value: Int, _left: Node = null, _right: Node = null, _random: Node = null) {
   var value: Int = _value
   var left: Node = _left
   var right: Node = _right
   var random: Node = _random
 }

 class NodeCopy(var _value: Int, var _left: NodeCopy = null, var _right: NodeCopy = null, var _random: NodeCopy = null) {
   var value: Int = _value
   var left: NodeCopy = _left
   var right: NodeCopy = _right
   var random: NodeCopy = _random
 }


object Solution {
  def copyRandomBinaryTree(root: Node): NodeCopy = {
    //val oldRefMap = new mutable.HashMap[Int,Node]()
    //val newRefMap = new mutable.HashMap[Int,NodeCopy]()
    val newRefMap = new mutable.HashMap[Int,NodeCopy]()

    def cloneNode(node : Node) : NodeCopy = {
      node match {
        case null => {
          null
        }
        case _ => {
          //oldRefMap += ((node.hashCode(),node))
          val leftClone = cloneNode(node.left)
          val rightClone = cloneNode(node.right)
          val newNode = new NodeCopy(node.value)
          newNode.left = leftClone
          newNode.right = rightClone


          newRefMap += ((node.hashCode(),newNode)) //map against old node hashcode vs newnode
          //newRefMap.addOne((newNode.value,newNode))
          newNode

        }
      }
    }

    def updateRandomNode(newNode : NodeCopy,oldNode : Node) : Unit = {
      newNode match {
        case null => {

        }
        case _ => {
          val randomOldNode = oldNode.random
          if (randomOldNode == null) {
            newNode.random = null
          }else {
            //val randomValue = randomOldNode.value
            //val oldHashCode = oldRefMap.get(randomOldNode.hashCode()).get
            val newNodeRandom = newRefMap.get(randomOldNode.hashCode()).get
            newNode.random = newNodeRandom

          }

          updateRandomNode(newNode.left,oldNode.left)
          updateRandomNode(newNode.right,oldNode.right)
        }
      }
    }

    val newRoot = cloneNode(root)
    updateRandomNode(newRoot,root)
    newRoot
  }
}