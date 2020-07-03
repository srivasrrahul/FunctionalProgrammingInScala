import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def distanceK(root: TreeNode, target: TreeNode, K: Int): List[Int] = {
    if (K == 0) {
      List(target.value)
    } else {
      val childDistanceMap = new mutable.HashMap[Int, List[Int]]() //nodeId,distance

      //K >= 1
      def distanceFromTarget(): Unit = {
        val q = new mutable.Queue[TreeNode]()
        if (target.left != null) {
          q.append(target.left)
        }

        if (target.right != null) {
          q.append(target.right)
        }

        var dist = 1
        while (q.isEmpty == false && dist < K) {
          val all = q.dequeueAll(_ => true)
          val lstBuffer = new ListBuffer[Int]
          for (x <- all) {
            lstBuffer.append(x.value)
            if (x.left != null) {
              q.append(x.left)
            }

            if (x.right != null) {
              q.append(x.right)
            }
          }

          childDistanceMap += ((dist, lstBuffer.toList))
          dist = dist + 1

        }

      }


      val rootPathMap = new mutable.HashMap[Int, (Int, TreeNode)]() //key is distance,value = (dir,node)

      def findDiff(current: TreeNode): Option[Int] = {
        current match {
          case null => {
            None
          }
          case _ => {
            if (current.value == target.value) {
              Some(0)
            } else {
              val leftDist = findDiff(current.left)
              val rightDist = findDiff(current.right)

              (leftDist, rightDist) match {
                case (None, None) => None
                case (Some(x), None) => {
                  //was found in left
                  rootPathMap += ((x + 1, (0, current)))
                  Some(x + 1)
                }
                case (None, Some(y)) => {
                  //was found in right
                  rootPathMap += ((y + 1, (1, current)))
                  Some(y + 1)
                }
                case _ => None //shud never enter here
              }
            }
          }
        }
      }

      val rootDiff = findDiff(root)

      val pendingVal = new mutable.HashMap[Int, List[Int]]()

      def processDifferent(current: TreeNode, distTillNow: Int): Unit = {
        current match {
          case null => {

          }
          case _ => {
            val existingLst = pendingVal.getOrElseUpdate(distTillNow, List())
            pendingVal += ((distTillNow, current.value :: existingLst))
            processDifferent(current.left, distTillNow + 1)
            processDifferent(current.right, distTillNow + 1)

          }
        }
      }

      for (parentValue <- rootPathMap) {
        val distance = parentValue._1
        val (dir, node) = parentValue._2
        if (dir == 0) {
          processDifferent(node.right, distance + 1)
        } else {
          processDifferent(node.left, distance + 1)
        }
      }

      //Now collect all as list
      val childDistanceLst = childDistanceMap.getOrElse(K, List())
      val parentLst = pendingVal.getOrElse(K, List())
      rootPathMap.get(K) match {

        case Some(item) => {
          item._2.value :: (childDistanceLst ++ parentLst)
        }
        case _ => {
          childDistanceLst ++ parentLst
        }
      }

    }
  }
}