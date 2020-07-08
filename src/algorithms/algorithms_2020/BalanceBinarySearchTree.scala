class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def balanceBST(root: TreeNode): TreeNode = {
    def createTree(lst : List[Int]) : TreeNode = {
      if (lst.isEmpty) {
        null
      }else {
        if (lst.size == 1) {
          new TreeNode(lst.head)
        }else {
          //println("Creatinh tree " + lst)
          val (firstHalf,secondHalf) = lst.splitAt(lst.length/2)

          val leftNode = createTree(firstHalf)
          val topNode = new TreeNode(secondHalf.head)
          val rightNode = createTree(secondHalf.tail)

          topNode.left = leftNode
          topNode.right = rightNode
          topNode
        }
      }

    }
    def balance(node : TreeNode) : List[Int] = {
      if (node == null) {
        (List())
      }else {
        val leftLst =  balance(node.left)
        val rightLst = balance(node.right)
        leftLst ++ List(node.value) ++ rightLst
      }
    }

    val lst = balance(root)
    createTree(lst)

  }
}