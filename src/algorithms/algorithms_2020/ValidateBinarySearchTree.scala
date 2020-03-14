class TreeNode(var _value: Int) {
   var value: Int = _value
   var left: TreeNode = null
   var right: TreeNode = null
 }

trait MinMaxValue
case class NodeValue(low : Int,high : Int) extends  MinMaxValue
case object NoBST extends MinMaxValue
case object NullNode extends MinMaxValue





object Solution {

  def isLeaf(node : TreeNode) : Boolean = {
    node.left == null && node.right == null
  }
  def itr(node : TreeNode) : MinMaxValue = {
    if (node == null) {
      NullNode
    }else {
      if (isLeaf(node)) {
        NodeValue(node.value, node.value)
      } else {
        val left = itr(node.left)
        val right = itr(node.right)

        (left,right) match {
          case (NullNode,NullNode) => new NodeValue(node.value,node.value)
          case (NoBST,_) => NoBST
          case (_,NoBST) => NoBST
          case (NullNode,n : NodeValue) => {
            if (node.value < n.low) {
              new NodeValue(node.value,n.high)
            }else {
              NoBST
            }
          }
          case (n : NodeValue,NullNode) => {
            if (node.value > n.high) {
              new NodeValue(n.low,node.value)
            }else {
              NoBST
            }
          }
          case (n1 : NodeValue,n2 : NodeValue) => {
            if (n1.high < node.value && node.value < n2.low) {
              new NodeValue(n1.low,n2.high)
            }else {
              NoBST
            }
          }
          case _ => NoBST
        }
      }
    }

  }
  def isValidBST(root: TreeNode): Boolean = {
    itr(root) match {
      case NoBST => false
      case _ => true
    }
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    val l1 = new TreeNode(1)
//    val l1_1 = new TreeNode(3)
//    val l2_1 = new TreeNode(0)


    root.left = l1
    //root.right = l1_1
    println(isValidBST(root))
  }
}