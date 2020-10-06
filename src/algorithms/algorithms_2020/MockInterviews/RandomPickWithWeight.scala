import scala.collection.mutable
import scala.util.Random

class TreeNode(val value : Int,val index : Int = -1,var left : TreeNode = null,var right: TreeNode = null) {

}
class Solution(_w: Array[Int]) {

  val pq = mutable.PriorityQueue.empty[TreeNode](new Ordering[TreeNode] {
    override def compare(x: TreeNode, y: TreeNode): Int = {
      y.value.compareTo(x.value)
    }
  })

  for (j <- 0 to _w.length-1) {
    pq.addOne(new TreeNode(_w(j),j))
  }

  for (j <- 0 to _w.length-2) {
    val i = pq.dequeue()
    val k = pq.dequeue()
    val node = new TreeNode(i.value + k.value)
    node.left = k
    node.right = i
    pq.addOne(node)
  }

  val root = pq.dequeue()


  // println(root.value)
  //  println(root.left)
  //  println(root.right)
  def pickIndex(): Int = {
    var current = root
    var foundIndex = -1

    while (foundIndex == -1) {
      if (current.left == null && current.right == null) {
        foundIndex = current.index
      }else {
        val random = Random.between(1, current.value + 1)
        if (random <= current.left.value ) {
          current = current.left
        }else {
          current = current.right
        }
      }
    }

    foundIndex
  }

}

// object MySolution {
//   def main(args: Array[String]): Unit = {
//     val s = new Solution(Array(20,70))
//     println(s.pickIndex())
//     println(s.pickIndex())
//     println(s.pickIndex())
//     println(s.pickIndex())
//     println(s.pickIndex())
//     println(s.pickIndex())
//     println(s.pickIndex())
//     println(s.pickIndex())



//   }
// }


/**
 * Your Solution object will be instantiated and called as such:
 * var obj = new Solution(w)
 * var param_1 = obj.pickIndex()
 */