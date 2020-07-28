

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def findTarget(root: TreeNode, k: Int): Boolean = {
    def findNode(node : TreeNode,needle : Int) : Boolean = {
      node match {
        case null => false
        case _ => {
          node.value == needle || findNode(node.left,needle) || findNode(node.right,needle)
        }
      }
    }

    def itr(node : TreeNode) : Boolean = {
      node match {
        case null => false
        case _ => {
          val pending = k - node.value
          if (node.value == pending) {
            itr(node.left) || itr(node.right)
          }else {
            findNode(root, pending) || itr(node.left) || itr(node.right)
          }
        }
      }
    }

    itr(root)
  }
}