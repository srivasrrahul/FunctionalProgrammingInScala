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
  def pathSum(root: TreeNode, sum: Int): List[List[Int]] = {
    //get all path from root till leaves and their sum and filter which is equal to sum
    //for root == null => its empty
    //lets start from 1,2,3,4 (1,2,4),(1,3)
    //1
    //2 3
    //4

    //for 4 => List(List(4))
    //for 2 => List(List(2,4))
    //for 1 => 1::List(2,4) + 1::List(3)
    def paths(currentNode : TreeNode) : List[List[Int]] = {
      currentNode match {
        case null => {
          List()
        }
        case _ => {
          //special for leaf??
          if (currentNode.left == null && currentNode.right == null) {
            List(List(currentNode.value))
          }else {
            val leftPaths = paths(currentNode.left)
            val rightPaths = paths(currentNode.right)

            val retValue = new scala.collection.mutable.ListBuffer[List[Int]]
            for (leftPath <- leftPaths) {
              retValue.append(currentNode.value :: leftPath)
            }

            for (rightPath <- rightPaths) {
              retValue.append(currentNode.value :: rightPath)
            }

            retValue.toList


          }
        }
      }
    }

    val retValue = paths(root)
    //println(retValue)
    retValue.filter(lst => lst.sum == sum)
  }
}