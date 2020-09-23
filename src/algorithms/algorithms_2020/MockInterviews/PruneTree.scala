/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
object Solution {
  def pruneTree(root: TreeNode): TreeNode = {
    def prune(node : TreeNode) : TreeNode = {
      if (node == null) {
        null
      }else {
        node.left = prune(node.left)
        node.right = prune(node.right)

        if (node.left != null  || node.right != null || node.value == 1) {
          node
        }else {
          null
        }
      }

    }

    prune(root)

  }
}