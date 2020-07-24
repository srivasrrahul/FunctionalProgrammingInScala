
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def distributeCoins(root: TreeNode): Int = {
    var totalMovement = 0
    def itr(node : TreeNode) : (Int,Int) = {
      //sub tree size and coin count
      if (node == null) {
        (0,0)
      }else {
        val (leftSubTreeSize,leftCoin) = itr(node.left)
        val (rightSubTreeSize,rightCoin) = itr(node.right)

        val leftDiff = math.abs(leftSubTreeSize - leftCoin) //this much needs to be trasnferred to/from left
        val rightDiff = math.abs(rightSubTreeSize - rightCoin) //this much needs to be trasnferred to/from left

        totalMovement = totalMovement + leftDiff + rightDiff
        (leftSubTreeSize + rightSubTreeSize + 1 ,leftCoin+rightCoin+node.value)
      }


    }

    itr(root)
    totalMovement
  }
}