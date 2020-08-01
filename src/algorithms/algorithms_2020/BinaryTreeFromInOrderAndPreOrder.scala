

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    def itr(x1 : Int,y1 : Int,x2 : Int,y2: Int) : TreeNode = {
      if (x1 == y1) {
        new TreeNode(preorder(x1))
      }else {
        var index = x2
        for (j <- x2 to y2 if index == -1) {
          if (inorder(j) == preorder(x1)) {
            index = j
          }
        }

        var leftNode : TreeNode = null
        var diff = 0
        if (index > x2) {
          diff = index-x2
          leftNode = itr(x1+1,x1+diff,index-diff,index-1)
        }

        var rightNode : TreeNode = null
        if (index+1 <= y2) {
          rightNode = itr(x1+diff+1,y1,index+1,y2)
        }

        new TreeNode(preorder(x1),leftNode,rightNode)
      }
    }

    if (inorder.isEmpty) {
      null
    }else {
      itr(0,preorder.length-1,0,inorder.length-1)
    }

  }
}