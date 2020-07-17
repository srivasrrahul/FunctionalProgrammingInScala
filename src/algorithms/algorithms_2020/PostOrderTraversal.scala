

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def postorderTraversal(root: TreeNode): List[Int] = {
    def postOrder(node : TreeNode) : List[Int] = {
      if (node == null) {
        List()
      }else {
        val left = postOrder(node.left)
        val right = postOrder(node.right)
        left ++ right ++ List(node.value)
      }
    }

    postorderTraversal(root)
  }
}