import scala.collection.mutable

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def pathSum(root: TreeNode, sum: Int): Int = {
    var totalCount = 0
    //val started = new mutable.HashSet[]()
    def itr(currentNode : TreeNode,pendingSum : Int) : Int = {
      if (currentNode == null) {
        0
      }else {
        if (pendingSum == currentNode.value) {
          totalCount = totalCount+1
        }

        itr(currentNode.left, pendingSum-currentNode.value)
        itr(currentNode.right, pendingSum-currentNode.value)

      }
    }

    def startPath(currentNode : TreeNode) :  Unit = {
      if (currentNode == null) {

      }else {
        itr(currentNode,sum)
        startPath(currentNode.left)
        startPath(currentNode.right)
      }
    }

    startPath(root)
    totalCount

  }
}