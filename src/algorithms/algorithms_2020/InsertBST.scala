class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
     var value: Int = _value
     var left: TreeNode = _left
     var right: TreeNode = _right
}

object Solution {
  def insertIntoBST(root: TreeNode, value : Int): TreeNode = {
    def insert(current : TreeNode) : TreeNode = {
      //shudn't be null
      current match {
        case null => {
          new TreeNode(value)
        }
        case _ => {
          if (current.value > value) {
            //insert at left
            current.left = insert(current.left)
          }else {
            current.right = insert(current.right)
          }

          current
        }
      }

    }

    if (root == null) {
      new TreeNode(value)
    }else {
      itr(root)
    }
  }
}