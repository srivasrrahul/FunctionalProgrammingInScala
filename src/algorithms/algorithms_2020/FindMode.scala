import scala.collection.mutable

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}
object Solution {
  def findMode(root: TreeNode): Array[Int] = {
    def mode(node : TreeNode) : (mutable.HashMap[Int,Int],mutable.TreeMap[Int,mutable.HashSet[Int]]) = {
      if (node == null) {
        (new mutable.HashMap[Int,Int](),new mutable.TreeMap[Int,mutable.HashSet[Int]]())
      }else {
        val (leftMode,leftModeTreeMap) = mode(node.left)
        val (rightMode,rightModeTreeMap) = mode(node.right)



        var leftCurrentCount = 0
        if (leftMode.contains(node.value)) {
          leftCurrentCount = leftMode.get(node.value).get
          leftMode.remove(node.value)

          val set = leftModeTreeMap.get(leftCurrentCount).get
          set.remove(node.value)

          if (set.size == 0) {
            leftModeTreeMap.remove(leftCurrentCount)
          }
        }

        var rightCurrentCount = 0
        if (rightMode.contains(node.value)) {
          rightCurrentCount = rightMode.get(node.value).get
          rightMode.remove(node.value)

          val set = rightModeTreeMap.get(rightCurrentCount).get
          set.remove(node.value)

          if (set.size == 0) {
            rightModeTreeMap.remove(rightCurrentCount)
          }
        }

        val currentCount = 1 + leftCurrentCount + rightCurrentCount


        val retValue = new mutable.HashMap[Int,Int]()
        retValue += ((node.value,currentCount))

        retValue.addAll(leftMode)
        retValue.addAll(rightMode)


        val treeMap = new mutable.TreeMap[Int,mutable.HashSet[Int]]()

        val s = treeMap.getOrElseUpdate(currentCount,new mutable.HashSet[Int]())
        s.add(node.value)

        for ((leftCount,leftSet) <- leftModeTreeMap) {
          val s = treeMap.getOrElseUpdate(leftCount,new mutable.HashSet[Int]())
          s.addAll(leftSet)
        }

        for ((rightCount,rightSet) <- rightModeTreeMap) {
          val s = treeMap.getOrElseUpdate(rightCount,new mutable.HashSet[Int]())
          s.addAll(rightSet)
        }


        (retValue,treeMap)




      }
    }

    if (root == null) {
      Array()
    }else {
      mode(root)._2.last._2.toArray
    }




  }
}