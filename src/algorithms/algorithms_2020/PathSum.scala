class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def hasPathSum(root: TreeNode, sum: Int): Boolean = {
    //for null root if sum == 0
    //for others iterate to leaf node and accmulate sum along the way if sum + leaf-node.value => increment-counter
    var sumCount = false
    def itr(currentNode : TreeNode,pathSum : Int) : Unit = {
      currentNode match {
        case null =>  {

        }
        case leaf if currentNode.left == null && currentNode.right == null => {
          //println("leaf")
          if ((pathSum + currentNode.value) == sum) {
            sumCount = true
          }
        }
        case _ => {
          itr(currentNode.left,pathSum + currentNode.value)
          itr(currentNode.right,pathSum + currentNode.value)
        }
      }
    }

    if (root == null) {
      false
    }else {
      itr(root,0)
      sumCount
    }
  }

}