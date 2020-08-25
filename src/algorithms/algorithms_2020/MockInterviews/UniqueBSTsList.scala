import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right

  override def toString = s"TreeNode($value, $left, $right)"
}

object Solution {
  def generateTrees(n: Int): List[TreeNode] = {
    def itr(begin : Int,end : Int) : List[TreeNode] = {
      if (begin == end) {
        List(new TreeNode(begin))
      }else {
        var trees = new ListBuffer[TreeNode]
        for (r <- begin to end) {
          var leftSubTree : List[TreeNode] = List()
          if (r > begin) {
            leftSubTree = itr(begin,r-1)
          }

          var rightSubTree : List[TreeNode] = List()
          if (r+1 <= end) {
            rightSubTree = itr(r+1,end)
          }

          if (leftSubTree.isEmpty) {
            for (right <- rightSubTree) {
              trees.append(new TreeNode(r,null,right))
            }
          }else {
            if (rightSubTree.isEmpty) {
              for (left <- leftSubTree) {
                trees.append(new TreeNode(r,left,null))
              }
            }else {
              for (left <- leftSubTree) {
                for (right <- rightSubTree) {
                  trees.append(new TreeNode(r,left,right))
                }
              }
            }
          }
        }

        trees.toList
      }
    }

    itr(1,n)
  }

  def main(args: Array[String]): Unit = {
    println(generateTrees(3))
  }
}