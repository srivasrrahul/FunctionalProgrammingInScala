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

    val queue = mutable.PriorityQueue.empty[TreeNode](Ordering.by((_ : TreeNode).value).reverse)

    if (root != null) {
      queue.addOne(root)
    }


    while (queue.isEmpty == false) {


      val maxValue = queue.head
      val allValues = queue.dequeueAll
      retValue.append(maxValue.value)
      for (value <- allValues) {
        if (value.left != null) {
          queue.addOne(value.left)
        }

        if (value.right != null) {
          queue.addOne(value.right)
        }
      }

    }

    retValue.toList
  }
}