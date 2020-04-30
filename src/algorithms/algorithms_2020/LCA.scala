import scala.util.control.Breaks._
class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
   var right: TreeNode = null
 }


object Solution {
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    def getPath(current : TreeNode,target : TreeNode,currentPath : List[TreeNode]) : (Boolean,List[TreeNode]) = {
      if (current == null) {
        (false,List[TreeNode]())
      }else {
        if (current.value == target.value) {
          (true, target :: currentPath)
        } else {
          val (leftPathExists, leftPath) = getPath(current.left, target, current :: currentPath)
          if (leftPathExists == false) {
            val (rightPathExists, rightPath) = getPath(current.right, target, current :: currentPath)
            if (rightPathExists == false) {
              (false, List[TreeNode]())
            } else {
              (rightPathExists, rightPath)
            }
          } else {
            (leftPathExists, leftPath)
          }
        }
      }
    }

    val pPath = getPath(root,p,List[TreeNode]())._2.reverse
    val qPath = getPath(root,q,List[TreeNode]())._2.reverse

//    println(pPath)
//    println(qPath)

    var prev = root
    breakable {
      for ((h1, h2) <- (pPath zip qPath)) {
        if (h1.value != h2.value) {
          break
        } else {
          prev = h1
        }
      }
    }

    prev
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    root.left = new TreeNode(2)
    root.right = new TreeNode(3)
    root.left.left = new TreeNode(4)
    root.left.right = new TreeNode(5)
    root.right.left = new TreeNode(6)
    root.right.right = new TreeNode(7)
    println(lowestCommonAncestor(root,root.left.left,root.right.left).value)
  }
}