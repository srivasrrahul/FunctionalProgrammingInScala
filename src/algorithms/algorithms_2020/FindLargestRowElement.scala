import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   var value: Int = _value
   var left: TreeNode = _left
   var right: TreeNode = _right
 }

object Solution {
  def largestValues(root: TreeNode): List[Int] = {
    val retValue = new ListBuffer[Int]

    val queue = new mutable.Queue[TreeNode]()
    if (root != null) {
      queue.append(root)
    }


    while (queue.isEmpty == false) {
      val allValues = queue.dequeueAll(_ => true)
      val maxValue = allValues.max(new Ordering[TreeNode] {
        override def compare(x: TreeNode, y: TreeNode): Int = {
          x.value.compareTo(y.value)
        }
      })
      retValue.append(maxValue.value)
      for (value <- allValues) {
        if (value.left != null) {
          queue.append(value.left)
        }

        if (value.right != null) {
          queue.append(value.right)
        }
      }

    }

    retValue.toList
  }
}