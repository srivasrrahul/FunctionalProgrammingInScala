import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Node(val index : Int,val indexVal : Int)
object NodeOrdering extends Ordering[Node] {
  override def compare(x: Node, y: Node): Int = {
    x.indexVal.compareTo(y.indexVal)
  }
}
object Solution {
  def maxSumAfterPartitioning(A: Array[Int], K: Int): Int = {
    val pq = mutable.PriorityQueue.empty[Node](NodeOrdering)
    for (j <- 0 to A.length-1) {
      val node = new Node(j,A(j))
      pq.addOne(node)
    }

    def findPossibleSets(source : Int,visited : Set[Int]) : List[List[Int]] = {
      val lstBuffer = new ListBuffer[List[Int]]
      def path(lst : List[Int]) : Unit = {
        if (lst.size <= K) {
          lstBuffer.append(lst)

          val first = lst.head
          val last = lst.last
          if (first-1 >=0 && visited.contains(first-1) == false) {
            path((first-1) :: lst)
          }

          if (last+1 < A.length && visited.contains(last+1) == false) {
            path(lst ++ List(last+1))
          }
        }


      }

      path(List(source))

      lstBuffer.toList
    }


    //println(pq)
    var maxSum = Int.MinValue
    def findSums(localPq : mutable.PriorityQueue[Node],visited : Set[Int],priorSum : Int) : Unit = {
      if (localPq.isEmpty == true) {
        //sumLst.append(priorSum)
        if (priorSum > maxSum) {
          maxSum = priorSum
        }
      }else {
        val top = localPq.dequeue()
        if (visited.contains(top.index)) {
          findSums(localPq.clone(),visited,priorSum)
        }else {
          val paths = findPossibleSets(top.index,visited)
          //println(paths)
          for (path <- paths) {
            val currentSum = top.indexVal*path.length
            findSums(localPq.clone(),visited.union(path.toSet),priorSum+currentSum)
          }
        }
      }
    }
    findSums(pq.clone(),Set(),0)

    //println(sumLst)
    maxSum

  }

  def main(args: Array[String]): Unit = {
    println(maxSumAfterPartitioning(Array(1,15,7,9,2,5,10),3))
  }
}