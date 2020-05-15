class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def sumNumbers(root: TreeNode): Int = {
    //if root == null then 0
    //else if node == leaf then form the number in the list and capture in the list and add to accuumulator which gets returned
    //  1
    //2   3
    //4 5   6
    var sum = 0
    def itr(currentNode : TreeNode,digitsCaptured : Int) : Unit = {
      currentNode match {
        case null => {

        }
        case leaf if currentNode.left == null && currentNode.right == null => {
          val digits = digitsCaptured * 10 + currentNode.value
          sum = sum + digits
        }
        case _ => {
          itr(currentNode.left,digitsCaptured*10 + currentNode.value)
          itr(currentNode.right,digitsCaptured*10 + currentNode.value)
        }
      }
    }

    itr(root,0)
    sum
  }
}