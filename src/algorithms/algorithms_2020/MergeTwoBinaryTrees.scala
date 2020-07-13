class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def mergeTrees(t1: TreeNode, t2: TreeNode): TreeNode = {
    def itr(node1 : TreeNode,node2 : TreeNode) : TreeNode = {
      (node1,node2) match {
        case (null,null) => null
        case (_,null) => node1
        case (null,_) => node2
        case (_,_) => {
          val newValue = node1.value + node2.value
          val left = itr(node1.left,node2.left)
          val right = itr(node1.right,node2.right)

          //use left
          node1.value = newValue
          node1.left = left
          node1.right = right
          node1
        }
      }
    }

    itr(t1,t2)
  }
}