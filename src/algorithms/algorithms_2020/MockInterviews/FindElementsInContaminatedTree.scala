

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

class FindElements(_root: TreeNode) {
  _root.value = 0
  def recover(node : TreeNode) : Unit = {
    //node is recovered
    if (node.left != null) {
      node.left.value = 2*node.value+1
      recover(node.left)
    }

    if (node.right != null) {
      node.right.value = 2*node.value + 2
      recover(node.right)
    }
  }
  def find(target: Int): Boolean = {
    def itr(node : TreeNode) : Boolean = {
      if (node == null) {
        false
      }else {
        if (node.value == target) {
          true
        }else {
          itr(node.left) || itr(node.right)
        }
      }
    }

    itr(_root)
  }

}