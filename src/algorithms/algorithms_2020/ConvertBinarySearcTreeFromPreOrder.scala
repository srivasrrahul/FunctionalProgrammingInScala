

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

//8,5,1,7,10,12
object Solution {
  def bstFromPreorder(preorder: Array[Int]): TreeNode = {
    def itr(currentIndex : Int,endIndex : Int) : TreeNode = {
      if (currentIndex > endIndex) {
        null
      }else {
        //exrtract till smaller
        var greaterThanIndex = currentIndex+1
        while (greaterThanIndex < preorder.length && preorder(greaterThanIndex) < preorder(currentIndex)) {
          greaterThanIndex = greaterThanIndex+1
        }

        var left = itr(currentIndex+1,greaterThanIndex-1)

        val right = itr(greaterThanIndex,endIndex)

        new TreeNode(preorder(currentIndex),left,right)
      }
    }

    itr(0,preorder.length-1)
  }
}