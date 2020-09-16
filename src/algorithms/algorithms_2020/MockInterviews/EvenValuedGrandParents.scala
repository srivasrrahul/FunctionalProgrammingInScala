

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def sumEvenGrandparent(root: TreeNode): Int = {
    var count = 0
    def itr(node : TreeNode,parent : TreeNode,grandParent : TreeNode) : Unit = {
      if (node == null) {

      }else {
        if (grandParent != null && grandParent.value % 2 == 0) {
          count = count + node.value
        }
        itr(node.left, node, parent)
        itr(node.right, node, parent)
      }
    }

    itr(root,null,null)
    count
  }
}