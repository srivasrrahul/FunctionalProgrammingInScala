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
  def pathSum(root: TreeNode, sum: Int): Int = {
    //check the path from a give node to its children and if true update the counter
    //kets try for tghis tree
    //   1
    // 2   -3
    //4  0 6  5

    //so consider 2 => (2),(2,0),(1,-3,5)
    //  1
    //    2


    var totalPathFound = 0
    def itr(currentNode : TreeNode,pendingSum : Int) : Unit = {
      currentNode match {
        case null => {

        }
        case _ => {
          //println(currentNode.value + " pending " + pendingSum)
          if (pendingSum == currentNode.value) {
            //println(currentNode.value + " == pendung === " + pendingSum)
            totalPathFound = totalPathFound + 1
          }
          itr(currentNode.left,pendingSum-currentNode.value)
          itr(currentNode.right,pendingSum-currentNode.value)

          //start its child nodes
          //println("refreshing " + currentNode.value)
        }
      }
    }

    def loop(currentNode : TreeNode) : Unit = {
      if (currentNode != null) {
        itr(currentNode,sum)
        loop(currentNode.left)
        loop(currentNode.right)
      }



    }
    loop(root)
    totalPathFound
  }
}