import scala.collection.mutable

class SnapshotArray(_length: Int) {
  val mutatedDataFromLastVersion = new mutable.TreeMap[Int,mutable.HashMap[Int,Int]]()(Ordering[Int].reverse)
  val latestCopy = new Array[Int](_length)
  val baseVersion = new Array[Int](_length)
  var version = 0
  def set(index: Int, v: Int) : Unit = {
    latestCopy(index) = v

    if (version > 0) {
      val changedInLatestMap = mutatedDataFromLastVersion.getOrElseUpdate(version,new mutable.HashMap[Int,Int]())
      changedInLatestMap += ((index,v))
    }
  }

  def snap(): Int = {
    if (version == 0) {
      Array.copy(latestCopy,0,baseVersion,0,latestCopy.length)
      version = 1
      0
    }else {
      val oldVersion = version
      version = version+1
      oldVersion
    }
  }

  def get(index: Int, snap_id: Int): Int = {
    println(mutatedDataFromLastVersion)
    if (snap_id == version) {
      latestCopy(index)
    }else {
      //get latest less than or equal to version
      val versionMap = mutatedDataFromLastVersion.rangeFrom(snap_id)
      println("For index " + index + " " + versionMap)
      if (versionMap.size > 0) {
        var found = false
        var retValue = -1
        for ((versionId,dataChanged) <- versionMap if found == false) {
          if (dataChanged.contains(index)) {
            retValue = dataChanged.get(index).get
            found = true
          }
        }

        if (found == false) {
          baseVersion(index)
        }else {
          retValue
        }
      }else {
        baseVersion(index)
      }
    }
  }

}