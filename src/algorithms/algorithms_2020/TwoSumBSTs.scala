class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def twoSumBSTs(root1: TreeNode, root2: TreeNode, target: Int): Boolean = {
    def convertToSet(node : TreeNode) : Set[Int] = {
      if (node == null) {
        Set()
      }else {
        val left = convertToSet(node.left)
        val right = convertToSet(node.right)
        left.+(node.value).union(right)
      }
    }

    val set = convertToSet(root2)
    def itr(node : TreeNode) : Boolean = {
      if (node == null) {
        false
      }else {
        val diff = target - node.value
        if (set.contains(diff)) {
          true
        }else {
          itr(node.left) | itr(node.right)
        }
      }
    }

    itr(root1)
  }
}