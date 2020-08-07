

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def isSameTree(p: TreeNode, q: TreeNode): Boolean = {
    def itr(n1 : TreeNode,n2 : TreeNode) : Boolean = {
      if (n1 == null && n2 == null) {
        true
      }else {
        if (n1 == null) {
          false
        }else {
          if (n2 == null) {
            false
          }else {
            n1.value == n2.value && itr(n1.left,n2.left) && itr(n1.right, n2.right)
          }
        }
      }
    }

    itr(p,q)
  }
}