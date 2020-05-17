class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def isBalanced(root: TreeNode): Boolean = {
    // tree is height balanced if
    //left-tree is balanced && rigt-tree is balance && abs(max-height(left)- max-height(right)) <= 1
    //root is balanced
    //1
    // 2  3
    //4 5   6
    def itr(current : TreeNode) : (Boolean,Int) = {
      current match {
        case null => (true,0)
        case _ => {
          val (isLeftBalanced,leftMaxHeight) = itr(current.left)
          val (isRightBalanced,rightMaxHeight) = itr(current.right)

          if (isLeftBalanced && isRightBalanced) {
            val diff = scala.math.abs(leftMaxHeight - rightMaxHeight)
            if (diff <= 1) {
              (true,scala.math.max(leftMaxHeight,rightMaxHeight)+1)
            }else {
              (false,-1)
            }
          }else {
            (false,-1)
          }
        }
      }
    }

    itr(root)._1
  }
}