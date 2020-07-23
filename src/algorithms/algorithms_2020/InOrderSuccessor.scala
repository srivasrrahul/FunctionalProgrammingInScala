

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}


object Solution {
  def inorderSuccessor(root: TreeNode, p: TreeNode): TreeNode = {
    def ifRightExists(node : TreeNode) : Boolean = {
      node.right != null
    }
    def itr(node : TreeNode,path : List[TreeNode]) : TreeNode = {
      if (node == null) {
        null
      }else {
        //println(node.value + " " + path + " " + p.value)
        if (node.value == p.value) {
          if (ifRightExists(node)) {
            var successor = node.right
            while (successor.left != null) {
              successor = successor.left
            }

            successor
          }else {
            var current = node
            var sucessor : TreeNode = null
            for (parent <- path if sucessor == null) {
              //println(parent)
              if (parent.left == current) {
                sucessor = parent
              }else {
                current = parent
              }
            }

            sucessor
          }
        }else {
          if (node.value > p.value) {
            itr(node.left,node :: path)
          }else {
            itr(node.right,node :: path)
          }
        }
      }
    }

    itr(root,List())
  }

  def main(args: Array[String]): Unit = {

  }
}