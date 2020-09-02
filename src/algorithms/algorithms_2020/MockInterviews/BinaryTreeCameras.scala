import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

case class Index(val code : Int,val parentWatched : Boolean,val mustAddRoot : Boolean)
object Solution {
  def minCameraCover(root: TreeNode): Int = {
    val cache = new mutable.HashMap[Index,Int]()
    def itr(node : TreeNode,parentWatched : Boolean,mustAddAtRoot : Boolean) : Int = {
      if (node == null) {
        0
      }else {
        val index = new Index(node.hashCode(),parentWatched,mustAddAtRoot)
        if (cache.contains(index)) {
          cache.get(index).get
        }else {
          val optionLst = new ListBuffer[Int]
          if (mustAddAtRoot) {
            //No other option
            optionLst.append(1 + itr(node.left, true, false) + itr(node.right, true, false))
          } else {
            //You can add at current
            optionLst.append(1 + itr(node.left, true, false) + itr(node.right, true, false))

            //You can ask left to add compulsiorily
            if (node.left != null) {
              optionLst.append(itr(node.left, false, true) + itr(node.right, false, false))
            }

            //You can ask right to add compulsiorily
            if (node.right != null) {
              optionLst.append(itr(node.left, false, false) + itr(node.right, false, true))
            }

            //Neither add at current and nor ask child if parent is watched
            if (parentWatched) {
              optionLst.append(itr(node.left, false, false) + itr(node.right, false, false))
            }



          }

          val minValue = optionLst.min
          cache += ((index, optionLst.min))
          //optionLst.min
          minValue
        }
      }
    }

    val a = itr(root,false,false)
    val b = itr(root,false,true)

    math.min(a,b)

  }
}