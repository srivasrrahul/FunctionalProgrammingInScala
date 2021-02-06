

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def trimBST(root: TreeNode, low: Int, high: Int): TreeNode = {
    def itr(node : TreeNode) : TreeNode = {
      if (node == null) {
        null
      }else {
        val leftTrimmed = itr(node.left)
        val rightTrimmed = itr(node.right)
        if (node.value >= low && node.value <= high) {
          node.left = leftTrimmed
          node.right = rightTrimmed
          node
        }else {
          if (leftTrimmed == null) {
            rightTrimmed
          }else {
            leftTrimmed
          }
        }
      }
    }

    itr(root)
  }
}