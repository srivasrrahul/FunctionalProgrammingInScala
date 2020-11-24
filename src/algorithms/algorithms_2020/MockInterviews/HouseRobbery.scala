import scala.collection.mutable.ArrayBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def rob(root: TreeNode): Int = {
    def itr(node : TreeNode,parentRobbed : Boolean) : Int = {
      var totalMoney = 0
      if (node == null) {
        0
      }else {
        val options = new ArrayBuffer[Int]()
        if (parentRobbed == false) {
          options.append(node.value + itr(node.left,true) + itr(node.right,true))
        }

        options.append(itr(node.left,false) + itr(node.right,false))

        options.max
      }

    }

    itr(root,false)
  }
}