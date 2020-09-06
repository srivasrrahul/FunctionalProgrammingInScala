class TreeNode(var x : Int,val left : TreeNode = null,val right : TreeNode = null) {
  var range : Range  = null

  override def toString = s"TreeNode($range, $x)"
}



object Solution {

  //based out of sum
  def createSegmentTree(arr : Array[Int]) : TreeNode = {
    def itr(b : Int,e : Int) : TreeNode = {
      if (b == e) {
        val leaf = new TreeNode(arr(b))
        leaf.range = Range(b,e+1)
        leaf
      }else {
        val mid = b + (e-b)/2
        val left = itr(b,mid)
        val right = itr(mid+1,e)

        val sum = left.x + right.x
        val node = new TreeNode(sum,left,right)
        node.range = Range(b,e+1)
        node
      }
    }

    itr(0,arr.length-1)
  }

  def findRangeSum(j : Int,k : Int,root : TreeNode) : Int = {
    val baseRange = Range(j,k+1)
    def itr(node : TreeNode) : Option[Int] = {
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

  def updateTree(index : Int,value : Int,root : TreeNode) : Unit = {
    val r = Range(index,index+1)
    def itr(node : TreeNode) : Int = {
      if (r == node.range) {
        //println("found")
        node.x = value
        node.x
      }else {
        val rightRange = node.right.range
        println(rightRange + " " + r.head)
        if (rightRange.contains(r.head)) {
          val updatedRight = itr(node.right)
          node.x = updatedRight + node.left.x
        }else {
          val updatedLeft = itr(node.left)
          node.x = updatedLeft + node.right.x
        }

        node.x
      }
    }

    itr(root)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(0,9,5,7,3)
    val root = createSegmentTree(arr)
    println(root)
    println(root.left)
    println(root.right)
    println(findRangeSum(1,2,root))
    updateTree(1,2,root)
    println(findRangeSum(1,2,root))
  }
}
