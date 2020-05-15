class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 var value: Int = _value
 var left: TreeNode = _left
 var right: TreeNode = _right
}

object Solution {
  def binaryTreePaths(root: TreeNode): List[String] = {
    //if current == null => ""
    //else path(left).foreach(x::_) + path(right).foreach(x::_)
    def itr(current : TreeNode) : List[String] = {
      current match {
        case null => List()
        case _ => {
          if (current.left == null && current.right == null) {
            List(current.value.toString)
          }else {
            val leftPaths = itr(current.left) //""
            val rightPaths = itr(current.right) //""

            val returnLst = new scala.collection.mutable.ListBuffer[String]
            val currentStr = current.value.toString
            for (path <- leftPaths) {
              returnLst.append(currentStr ++ "->" + path)
            }

            for (path <- rightPaths) {
              returnLst.append(currentStr ++ "->" + path)
            }

            returnLst.toList
          }
        }
      }
    }

    itr(root)
  }
}