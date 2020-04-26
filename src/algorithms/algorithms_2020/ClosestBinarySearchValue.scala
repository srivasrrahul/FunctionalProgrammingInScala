
class TreeNode(var _value: Int) {
 var value: Int = _value
 var left: TreeNode = null
 var right: TreeNode = null
}

object Solution {
  def closestValue(root: TreeNode, target: Double): Int = {
    def explore(node : TreeNode,closestValueTillNow : Int) : Int = {
      if (node == null) {
        closestValueTillNow
      }else {
        var updatedClosesetValue = if (closestValueTillNow == -1) node.value else  closestValueTillNow
        val currentDiff = scala.math.abs(node.value.toDouble - target)

        if (currentDiff < scala.math.abs(closestValueTillNow-target)) {
          updatedClosesetValue = node.value
        }

        if (target < node.value) {
          explore(node.left,updatedClosesetValue)
        }else {
          explore(node.right,updatedClosesetValue)
        }

      }
    }

    explore(root,-1)
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(4)
    val l = new TreeNode(2)
    val r = new TreeNode(5)

    val ll = new TreeNode(1)
    val lr = new TreeNode(3)

    root.left = l
    root.right = r

    l.left = ll
    l.right = lr

    println(closestValue(root,1.75))
  }
}