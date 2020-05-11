class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
     var value: Int = _value
     var left: TreeNode = _left
     var right: TreeNode = _right
}

  object Solution {
    def sortedArrayToBST(nums: Array[Int]): TreeNode = {
      def itr(begin : Int,end : Int) : TreeNode = {
        if (begin == end) {
          new TreeNode(nums(begin))
        }else {
          if (begin > end) {
            null
          }else {
            val mid = begin + (end-begin)/2
            val left = itr(begin,mid-1)
            val right = itr(mid+1,end)
            new TreeNode(nums(mid),left,right)
          }
        }
      }

      itr(0,nums.length-1)
    }

    def main(args: Array[String]): Unit = {

    }
}