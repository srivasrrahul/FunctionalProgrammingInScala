class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def invertTree(root: TreeNode): TreeNode = {
    def invert(node : TreeNode) : TreeNode = {
      if (node == null) {
        null
      }else {
        val leftInverted = invert(node.left)
        val rightInverted = invert(node.right)
        node.left = rightInverted
        node.right = leftInverted
        node
      }
    }

    invert(root)
  }
}