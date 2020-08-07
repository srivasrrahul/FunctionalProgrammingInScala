import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def levelOrderBottom(root: TreeNode): List[List[Int]] = {
    val q = new mutable.Queue[TreeNode]()
    if (root != null) {
      q.addOne(root)
    }

    val lstBuffer = new ListBuffer[List[Int]]
    while (q.isEmpty == false) {
      val tops = q.dequeueAll(_ => true)
      val localLst = new ListBuffer[Int]
      for (top <- tops) {
        localLst.append(top.value)
        if (top.left != null) {
          q.addOne(top.left)
        }

        if (top.right != null) {
          q.addOne(top.right)
        }
      }

      lstBuffer.append(localLst.toList)

    }

    lstBuffer.reverse.toList

  }
}