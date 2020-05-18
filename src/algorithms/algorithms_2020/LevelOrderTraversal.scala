/**
 * Definition for a binary tree node.
 * class TreeNode(var _value: Int) {
 *   var value: Int = _value
 *   var left: TreeNode = null
 *   var right: TreeNode = null
 * }
 */
class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def levelOrder(root: TreeNode): List[List[Int]] = {
    val lstBuffer = new scala.collection.mutable.ListBuffer[List[Int]]
    val q = new scala.collection.mutable.Queue[TreeNode]

    if (root != null) {
      q.addOne(root)
    }
    while (q.isEmpty == false) {
      val elements = q.dequeueAll(_ => true)
      lstBuffer.append(elements.map(node => node.value).toList)

      for (element <- elements) {
        if (element != null) {
          if (element.left != null) {
            q.addOne(element.left)
          }

          if (element.right != null) {
            q.addOne(element.right)
          }
        }
      }

    }

    lstBuffer.toList


  }
}