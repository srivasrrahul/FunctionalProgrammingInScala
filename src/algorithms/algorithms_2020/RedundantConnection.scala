object Solution {
  def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] = {
    var redundantConnection : Option[Array[Int]] = None
    val nodeGroup = new scala.collection.mutable.HashMap[Int,Int] //node id to group id
    val groupMap = new scala.collection.mutable.HashMap[Int,scala.collection.mutable.HashSet[Int]] //group id to all elements in group
    var groupIdGen = 0
    def getNewGroupId() : Int = {
      groupIdGen = groupIdGen + 1
      groupIdGen
    }
    for (edge <- edges) {
      //println("For edge " + (edge.mkString(",")) + " " + groupMap)
      val source = edge(0)
      val dest = edge(1)

      val sourceGroupId = nodeGroup.get(source)
      val destGroupId = nodeGroup.get(dest)

      (sourceGroupId,destGroupId) match {
        case (None,None) => {
          val sourceGroupNewId = getNewGroupId()
          val destGroupNewId = sourceGroupNewId

          nodeGroup += ((source,sourceGroupNewId))
          nodeGroup += ((dest,destGroupNewId))

          val combinedSet = new scala.collection.mutable.HashSet[Int]
          combinedSet.add(source)
          combinedSet.add(dest)
          groupMap += ((sourceGroupNewId,combinedSet))
          groupMap += ((destGroupNewId,combinedSet))
        }
        case (None,Some(destGroupExistingId)) => {
          nodeGroup += ((source,destGroupExistingId))
          groupMap.get(destGroupExistingId).get.add(source)
        }
        case (Some(sourceGroupExistingId),None) => {
          nodeGroup += ((dest,sourceGroupExistingId))
          groupMap.get(sourceGroupExistingId).get.add(dest)
        }
        case (Some(sourceGroupExistingId),Some(destGroupExistingId)) => {
          if (sourceGroupExistingId == destGroupExistingId) {
            redundantConnection = Some(edge)
          }else {
            //merge them
            val newId = getNewGroupId()
            val sourceGroupNeigbours = groupMap.get(sourceGroupExistingId).get
            val destGroupNeigbours = groupMap.get(destGroupExistingId).get
            val combinedSet = sourceGroupNeigbours.union(destGroupNeigbours)

            groupMap += ((newId,combinedSet))
            groupMap.remove(sourceGroupExistingId)
            groupMap.remove(destGroupExistingId)
            for (combinedNeigbour <- combinedSet) {
              nodeGroup += ((combinedNeigbour,newId))
            }

          }
        }
      }

    }

    //println(groupMap)
    redundantConnection.get //can't be none
  }
}