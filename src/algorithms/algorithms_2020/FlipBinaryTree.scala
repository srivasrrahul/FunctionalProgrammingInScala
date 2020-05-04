

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 var value: Int = _value
 var left: TreeNode = _left
 var right: TreeNode = _right
}

object Solution {
  def flipEquiv(root1: TreeNode, root2: TreeNode): Boolean = {
    def check(node1: TreeNode, node2: TreeNode): Boolean = {
      (node1, node2) match {
        case (null, null) => true
        case (null, _) => false
        case (_, null) => false
        case (_, _) => {
          if (node1.value != node2.value) {
            false
          } else {
            val check1 = check(node1.left, node2.left)
            val check2 = check(node1.left, node2.right)
            if (check1 && check2) {
              true
            }else {
              val check3 = check(node1.right, node1.right)
              val check4 = check(node1.right, node1.left)
              if (check3 && check4) {
                true
              }else {
                false
              }
            }
          }
        }
      }
    }

    check(root1,root2)
  }


}