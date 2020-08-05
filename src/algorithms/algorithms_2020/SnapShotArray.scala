import scala.collection.mutable

case class VersionData(val arrIndex : Int,version : Int)
class SnapshotArray(_length: Int) {
  val data = new mutable.HashMap[Int,Array[Int]]()
  val arr = new Array[Int](_length)
  var version = 0
  def set(index: Int, v: Int) {
    arr(index) = v
  }

  def snap(): Int = {
    data += ((version,arr.clone()))
    val oldVersion = version
    version = version+1
    oldVersion
  }

  def get(index: Int, snap_id: Int): Int = {
    if (snap_id == version) {
      arr(index)
    }else {
      data.get(snap_id).get(index)
    }
  }

}