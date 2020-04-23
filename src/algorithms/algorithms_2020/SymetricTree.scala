
 class TreeNode(var _value: Int) {
   var value: Int = _value
   var left: TreeNode = null
   var right: TreeNode = null
 }


object Solution {
  def isSymmetric(root: TreeNode): Boolean = {
    def explore(current : TreeNode,cousin : TreeNode) : Boolean = {
      if (current == null && cousin == null) {
        true
      }else {
       if (current == null) {
         false
       }else {
         if (cousin == null) {
           false
         }else {
           explore(current.left,cousin.right) && explore(current.right,cousin.left) && current.value == cousin.value
         }
       }
      }
    }

    if (root == null) {
      true
    }else {
      explore(root.left,root.right)
    }

  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    val l = new TreeNode(2)
    val r = new TreeNode(2)
    val ll = new TreeNode(3)
    val lr = new TreeNode(4)
    val rl = new TreeNode(4)
    val rr = new TreeNode(3)

    root.left = l
    root.right = r

    l.left = ll
    l.right = lr

    r.left = rl
    r.right = rr

    println(isSymmetric(null))
  }
}