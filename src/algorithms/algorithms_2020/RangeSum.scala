class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  def rangeSumBST(root: TreeNode, L: Int, R: Int): Int = {
    var totalSum = 0
    def explore(current : TreeNode) : Unit = {
      if (current != null) {
        if (current.value >= L && current.value <= R) {
          totalSum += current.value
        }

        if (current.value >= L) {
          explore(current.left)
        }

        if (current.value <= R) {
          explore(current.right)
        }
      }
    }

    explore(root)

    totalSum
  }


}