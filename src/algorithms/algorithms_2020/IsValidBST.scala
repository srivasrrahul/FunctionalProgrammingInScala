/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
object Solution {
  def isValidBST(root: TreeNode): Boolean = {
    //returns min and max and if a tree is BST
    def itr(current : TreeNode) : (Boolean,Option[(Int,Int)]) = {
      current match {
        case null => {
          (true,None)
        }
        case _ => {
          //val leftResult = itr(current.left)
          //val rightResult = itr(current.right)
          //println("For val " + current.value + " " + leftResult + "  " + rightResult)
          (itr(current.left),itr(current.right)) match {
            case ((false,_),_) => (false,None)
            case (_,(false,_)) => (false,None)
            case ((true,None),(true,None)) => (true,Some((current.value,current.value)))
            case ((true,Some((minLeft,maxLeft))),(true,None)) => {
              if (current.value > maxLeft) {
                (true,Some(minLeft,current.value))
              }else {
                (false,None)
              }
            }
            case ((true,None),(true,Some((minRight,maxRight)))) => {
              if (current.value < minRight) {
                (true,Some(current.value,maxRight))
              }else {
                (false,None)
              }
            }
            case ((true,Some((minLeft,maxLeft))),(true,Some((minRight,maxRight)))) => {
              if (current.value > maxLeft && current.value < minRight) {
                (true,Some(minLeft,maxRight))
              }else {
                (false,None)
              }
            }
          }
        }
      }
    }

    itr(root)._1
  }
}