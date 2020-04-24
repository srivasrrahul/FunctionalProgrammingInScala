class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  def flatten(root: TreeNode): Unit = {
    def explore(node : TreeNode) : (TreeNode,TreeNode) = {
      if (node == null) {
        (null,null)
      }else {
        if (node.left != null) {
          val saveRight = node.right
          val (nodeLeftBegin,nodeLeftEnd) = explore(node.left)

          node.right = nodeLeftBegin

          nodeLeftEnd.right = saveRight
          node.left = null

          var lastNode = nodeLeftEnd
          if (nodeLeftEnd.right != null) {
            val (_,nodeRightLast) = explore(nodeLeftEnd.right)
            if (nodeRightLast != null) {
              lastNode = nodeRightLast
            }
          }

          (node,lastNode)
        }else {
          if (node.right == null) {
            (node,node)
          }else {
            val (_,rightMostNode) = explore(node.right)
            (node,rightMostNode)
          }
        }
      }
    }

    explore(root)._1
  }
}