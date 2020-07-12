
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def isValidSequence(root: TreeNode, arr: Array[Int]): Boolean = {
    def isLeaf(node : TreeNode) : Boolean = {
      node != null && node.left == null && node.right == null
    }
    def itr(node : TreeNode,arrIndex : Int) : Boolean = {
      if (arrIndex == arr.length-1) {
        if (isLeaf(node) && node.value == arr(arrIndex)) {
          true
        }else {
          false
        }
      }else {
        if (node == null) {
          false
        }else {
          if (node.value == arr(arrIndex)) {
            itr(node.left,arrIndex+1) || itr(node.right,arrIndex+1)
          }else {
            false
          }
        }
      }
    }

    itr(root,0)
  }

  def main(args: Array[String]): Unit = {

  }
}