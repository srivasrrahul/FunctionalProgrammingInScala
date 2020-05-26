import scala.collection.mutable

case class TreeNode(val x : String, val children : mutable.HashSet[TreeNode] = new mutable.HashSet[TreeNode](), var parent : TreeNode = null)
object Solution {
  def findSmallestRegion(regions: List[List[String]], region1: String, region2: String): String = {
    val globalIndex = new mutable.HashMap[String,TreeNode]()

    for (region <- regions) {
      val head = region.head
      val headNode = globalIndex.getOrElseUpdate(head,new TreeNode(head))

      for (subRegion <- region.tail) {
        val subRegionNode = globalIndex.getOrElseUpdate(subRegion,new TreeNode(subRegion))
        headNode.children.add(subRegionNode)
        subRegionNode.parent = headNode
      }
    }

    var r1Node = globalIndex.get(region1).get
    var r2Node = globalIndex.get(region2).get

    var region1ToRoot = List[String]()
    while (r1Node != null) {
      println(r1Node.x)
      region1ToRoot = r1Node.x:: region1ToRoot
      r1Node = r1Node.parent
    }

    var region2ToRoot = List[String]()
    while (r2Node != null) {
      println(r2Node.x)
      region2ToRoot = r2Node.x :: region2ToRoot
      r2Node = r2Node.parent
    }


//    println(region1ToRoot)
//    println(region2ToRoot)

    var varianceFound = false
    var current = region1ToRoot.head
    for ((x,y) <- region1ToRoot zip region2ToRoot if varianceFound == false) {
      if (x != y) {
        varianceFound = true
      }else {
        current = x
      }
    }

    current




  }

  def main(args: Array[String]): Unit = {
    println(findSmallestRegion(List(List("Earth","North America","South America"),
      List("North America","United States","Canada"),
      List("United States","New York","Boston"),
      List("Canada","Ontario","Quebec"),
      List("South America","Brazil")),"Canada","United States"))
  }
}