
class TreeNode(var _value: Int) {
   var value: Int = _value
   var left: TreeNode = null
   var right: TreeNode = null
 }

object Solution {
    def size(root : TreeNode) : Int = {
      root match {
        case null => 0
        case _ => {
          1 + size(root.left) + size(root.right)
        }
      }
    }
    def kthSmallest(root: TreeNode, k: Int): Int = {
      val left_size = size(root.left)
      val right_size = size(root.right)

      if (k == left_size+1) {
        root._value
      }else {
        if (k <= left_size) {
          kthSmallest(root.left,k)
        }else {
          kthSmallest(root.right,k-(left_size+1))
        }
      }

    }
}