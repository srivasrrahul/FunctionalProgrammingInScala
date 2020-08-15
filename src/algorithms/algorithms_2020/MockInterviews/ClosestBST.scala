
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def closestValue(root: TreeNode, target: Double): Int = {
    var closestValue : Option[(Int,Double)] = None
    def itr(node : TreeNode) : Unit = {
      if (node == null) {

      }else {
        val absDiff = scala.math.abs(node.value-target)
        closestValue match {
          case None => {
            closestValue = Some((node.value,absDiff))
          }
          case Some((existingValue,existingDiff)) => {
            if (absDiff < existingDiff) {
              closestValue = Some((node.value,absDiff))
            }
          }
        }

        itr(node.left)
        itr(node.right)
      }
    }

    itr(root)
    closestValue.get._1
  }
}