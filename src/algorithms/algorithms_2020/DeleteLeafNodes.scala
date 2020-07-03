class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def removeLeafNodes(root: TreeNode, target: Int): TreeNode = {
    def deleteNode(node : TreeNode) : TreeNode = {
      node match {
        case null => {
          null
        }
        case _ => {
          node.left = deleteNode(node.left)
          node.right = deleteNode(node.right)

          if (node.left == null && node.right == null && node.value == target) {
            null
          }else {
            node
          }
        }
      }
    }

    deleteNode(root)
  }
}