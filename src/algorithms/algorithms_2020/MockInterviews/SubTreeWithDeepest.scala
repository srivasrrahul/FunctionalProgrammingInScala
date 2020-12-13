

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def subtreeWithAllDeepest(root: TreeNode): TreeNode = {
    def itr(node : TreeNode,depth : Int) : (TreeNode,Int) = {
      if (node == null) {
        (null,depth)
      }else {
        if (node.left == null && node.right == null) {
          //leaf node
          (node,depth)
        }else {

          val (leftDeepest,leftLen) = itr(node.left,depth+1)
          val (rightDeepest,rightLen) = itr(node.right,depth+1)

          (leftDeepest,rightDeepest) match {
            case (null,_) => {
              (rightDeepest,rightLen)
            }
            case (_,null) => {
              (leftDeepest,leftLen)
            }
            case (_,_) => {
              if (leftLen == rightLen) {
                (node,leftLen)
              }else {
                if (leftLen > rightLen) {
                  (leftDeepest,leftLen)
                }else {
                  (rightDeepest,rightLen)
                }
              }
            }
          }

        }
      }
    }

    val (deepest,_) = itr(root,0)
    deepest
  }
}