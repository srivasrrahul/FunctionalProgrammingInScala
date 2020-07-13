class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def deleteNode(root: TreeNode, key: Int): TreeNode = {
    def delete(node : TreeNode, localKey : Int) : TreeNode = {
      if (node == null) {
        null
      }else {

        if (node.value == key) {
          val left = node.left
          val right = node.right
          (left,right) match {
            case (null,null) => null
            case (_,null) => left
            case (null,_) => right
            case (_,_) => {

              //find largest in left tree
              //use that to form new root
              //right will be right of largest
              var leftLargest = left
              var leftLargestParent = node
              while (leftLargest.right != null) {
                leftLargest = leftLargest.right
              }

              val deletedPendingLeftLargestNode = delete(node,leftLargest.value)
              leftLargest.left = deletedPendingLeftLargestNode
              leftLargest.right = right
              leftLargest



            }
          }
        }else {
          if (node.value < localKey) {
            delete(node.right,localKey)
          }else {
            delete(node.left,localKey)
          }
        }
      }
    }

    delete(root,key)
  }
}