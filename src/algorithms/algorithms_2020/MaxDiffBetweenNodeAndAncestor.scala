import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def maxAncestorDiff(root: TreeNode): Int = {
    def itr(node : TreeNode) : List[(Int,Int)] = {
      if (node == null) {
        List()
      }else {
        val leftMinMaxLst = itr(node.left)
        val righrMinMaxLst = itr(node.right)


        val retValue = new ListBuffer[(Int,Int)]
        for ((minValue,maxValue) <- (leftMinMaxLst ++ righrMinMaxLst)) {
          if (node.value < minValue) {
            retValue.append((node.value,maxValue))
          }else {
            if (node.value > maxValue) {
              retValue.append((minValue,node.value))
            }else {
              retValue.append((minValue,maxValue))
            }
          }
        }

        if (retValue.isEmpty) {
          retValue.append((node.value,node.value))
        }

        retValue.toList
      }
    }

    val minMaxLst = itr(root)

    val maxDiff = minMaxLst.max(new Ordering[(Int,Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = {
        val diff1 = math.abs(x._1 - x._2)
        val diff2 = math.abs(y._1 - y._2)
        diff1.compareTo(diff2)
      }
    })

    math.abs(maxDiff._1-maxDiff._2)
  }
}