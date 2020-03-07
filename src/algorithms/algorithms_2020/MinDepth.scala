class TreeNode(var _value: Int) {
 var value: Int = _value
 var left: TreeNode = null
 var right: TreeNode = null
 }

object Solution {
  def isLeaf(node : TreeNode) : Boolean ={
    node.left == null && node.right == null
  }
  def minDepth(root: TreeNode): Int = {
    def itr(node : TreeNode) : Int = {
      if (node == null) {
        0
      }else {
        if (isLeaf(node)) {

          //println("Node is " + node.value)
          1
        }else {

          var left_depth = 0
          if (node.left != null) {
            left_depth = 1 + itr(node.left)
          }

          var right_depth = 0
          if (node.right != null) {
            right_depth = 1 + itr(node.right)
          }

          if (left_depth != 0 && right_depth != 0) {
            scala.math.min(left_depth,right_depth)
          }else {

            if (left_depth == 0) {
              right_depth
            } else {
              left_depth
            }
          }
        }
      }

    }

    itr(root)
  }


  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    val l1_1 = new TreeNode(2)
    val l1_2 = new TreeNode(3)
    val l2_1 = new TreeNode(4)
    val l2_2 = new TreeNode(5)

    root.left = l1_1
    root.right = l1_2

    l1_1.left = l2_1
    l1_1.right = l2_2

    println(minDepth(root))
  }
}