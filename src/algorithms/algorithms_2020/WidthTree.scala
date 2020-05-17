/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}


object Solution {
  def widthOfBinaryTree(root: TreeNode): Int = {
    val q = new scala.collection.mutable.Queue[TreeNode]
    var maxWidth = Int.MinValue
    if (root != null) {
      q.addOne(root)
      maxWidth = 1
    }


    while (q.isEmpty == false) {

      var dequeuedLst = q.dequeueAll(_ => true)

      //trip left and right nulls
      //val newLst = new scala.collection.mutable.ListBuffer[TreeNode]
      var initEncountered = false

      for (element <- dequeuedLst) {
        if (element != null) {
          if (element.left != null) {
            //newLst.append(element.left)
            q.append(element.left)
            initEncountered = true
          }else {
            if (initEncountered == true) {
              //newLst.append(element.left)
              q.append(element.left)
            }
          }

          if (element.right != null) {
            //newLst.append(element.right)
            q.append(element.right)
            initEncountered = true
          }else {
            if (initEncountered == true) {
              //newLst.append(element.right)
              q.append(element.right)
            }
          }
        }else {
          if (initEncountered == true) {
            //newLst.append(null)
            //newLst.append(null)
            q.append(null)
            q.append(null)
          }
        }
      }

      //trim from right
      while (q.isEmpty == false && q.last == null) {
        q.dropRightInPlace(1)
      }

      if (q.size > maxWidth) {
        maxWidth = q.size
      }



    }

    if (maxWidth == Int.MinValue) {
      0
    }else {
      maxWidth
    }

  }
}