class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def splitBST(root: TreeNode, V: Int): Array[TreeNode] = {
    def split(node : TreeNode) : (TreeNode,TreeNode) = {
      if (node == null) {
        (null,null)
      }else {
        if (node.value <= V) {
          val (rightLess,rightMore) = split(node.right)
          node.right = rightLess
          (node,rightMore)
        }else {
          val (leftLess,leftMore) = split(node.left)
          node.left = leftMore
          (leftLess,node)
        }
      }
    }

    val (less,more) = split(root)
    Array(less,more)
  }
}