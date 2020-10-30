

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}



class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
object Solution {
  def isSubPath(head: ListNode, root: TreeNode): Boolean = {
    def checkContinuos(node : TreeNode,lstNode : ListNode) : Boolean = {
      if (node == null && lstNode == null) {
        true
      }else {
        if (node == null) {
          false
        }else {
          if (lstNode == null) {
            true
          }else {
            if (node.value != lstNode.x) {
              false
            }else {
              checkContinuos(node.left,lstNode.next) || checkContinuos(node.right,lstNode.next)
            }
          }
        }
      }
    }
    def check(node : TreeNode) : Boolean = {
      if (node == null) {
        false
      }else {
        checkContinuos(node,head) || check(node.left) || check(node.right)
      }
    }

    check(root)
  }
}