class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def goodNodes(root: TreeNode): Int = {
    def path(node : TreeNode,maxValueTillNow : Int) : Int = {
      if (node == null) {
        0
      }else {
        var countSelf = 0
        var currentMax : Int = maxValueTillNow
        if (node.value > maxValueTillNow) {
          countSelf = 1
          currentMax = node.value
        }

        val leftPath = path(node.left,currentMax)
        val rightPath = path(node.right,currentMax)

        leftPath + rightPath + countSelf
      }
    }

    if (root != null) {
      path(root.left,root.value) + path(root.right,root.value)
    }else {
      0
    }

  }
}