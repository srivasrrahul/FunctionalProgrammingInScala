/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
}
object Solution {
  def diameterOfBinaryTree(root: TreeNode): Int = {
    def itr(current : TreeNode) : (Int,Int) = {
      //return depth and diameter locally
      //current does it for left and right
      //check if left and right depth is greater than given diamerters from both end update end
      //else max diameter moves up
      //example [1,2,3,4] =>
      //itr(1) =
      current match {
        case null => {
          (0,0)
        }
        case _ => {
          val (leftMaxDepth,leftDiameter) = itr(current.left)
          val (rightMaxDepth,rightDiameter) = itr(current.right)

          val newDiameter = leftMaxDepth + rightMaxDepth + 1

          val maxDiaemeter = scala.math.max(newDiameter,scala.math.max(leftDiameter,rightDiameter))

          val maxDepth = scala.math.max(leftMaxDepth,rightMaxDepth)

          (maxDepth+1,maxDiaemeter)
        }
      }
    }

    val res = itr(root)._2
    if (res > 0) {
      res-1
    }else {
      0
    }
  }
}