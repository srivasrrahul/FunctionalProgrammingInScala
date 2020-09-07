class TreeNode(var x : Long,val left : TreeNode = null,val right : TreeNode = null) {
  var range : Range  = null

  override def toString = s"TreeNode($range, $x)"
}
class SegTreeWrapper(val sumRange: Range) {
  var count = 0

  def createSegmentTree(arr : Array[Int]) : TreeNode = {
    def itr(b : Int,e : Int) : TreeNode = {
      if (b == e) {
        val leaf = new TreeNode(arr(b))
        leaf.range = Range(b,e+1)
        if (sumRange.contains(leaf.x)) {
          count = count+1
        }
        leaf
      }else {
        val mid = b + (e-b)/2
        val left = itr(b,mid)
        val right = itr(mid+1,e)

        val sum = left.x + right.x
        val node = new TreeNode(sum,left,right)
        node.range = Range(b,e+1)

        if (sumRange.contains(node.x)) {
          count = count+1
        }
        node
      }
    }

    itr(0,arr.length-1)
  }

  def findRangeSum(j : Int,k : Int,root : TreeNode) : Long = {
    val baseRange = Range(j,k+1)
    def itr(node : TreeNode) : Option[Long] = {
      if (node == null) {
        None
      }else {
        //full one
        //println(node.range + " " + baseRange)
        if (node.range.head >= baseRange.head && node.range.last <= baseRange.last) {
          //Full
          Some(node.x)
        }else {
          if ((baseRange.head <= node.range.last && baseRange.last > node.range.last) ||
            (baseRange.head < node.range.head && baseRange.last >= node.range.head) ||
            (baseRange.head >= node.range.head && baseRange.last <= node.range.last)){
            val left = itr(node.left)
            val right = itr(node.right)
            (left,right) match {
              case (None,None) => None
              case (Some(lSum),None) => {
                Some(lSum)
              }
              case (None,Some(rSum)) => {
                Some(rSum)
              }
              case (Some(lSum),Some(rSum)) => {
                Some(lSum + rSum)
              }
            }
          }else {
            None
          }
        }
      }

    }

    itr(root).get
  }
}
object Solution {


  def countRangeSum(nums: Array[Int], lower: Int, upper: Int): Int = {
    if (nums.length == 0) {
      0
    }else {
      val wrapper = new SegTreeWrapper(Range(lower,upper+1))
      val root = wrapper.createSegmentTree(nums)
      println(root)
      println(root.left)
      println(root.right)
      val baseRange = Range(lower,upper+1)
      //println(baseRange)
      var count = 0
      for (j <- 0 to nums.length-1) {
        for (k <- j to nums.length-1) {
          val rangeSum =   wrapper.findRangeSum(j,k,root)
          println("Range sum " + j + " " + k + " " + rangeSum)
          if (baseRange.contains(rangeSum)) {
            count = count + 1
          }
        }
      }
      count
    }
  }
}