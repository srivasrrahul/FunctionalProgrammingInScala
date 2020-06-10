import scala.collection.mutable

object Solution {
  def isIsomorphic(s: String, t: String): Boolean = {
    val maping = new mutable.HashMap[Char,Char]()
    val takenMapping = new mutable.HashSet[Char]

    var isomorphic = true
    for ((x,y) <- s zip t if isomorphic == true) {
      maping.get(x) match {
        case None => {
          if (takenMapping.contains(y)) {
            isomorphic = false
          }else {
            maping += ((x, y))
            takenMapping.add(y)
          }
        }
        case Some(z) => {
          if (y != z) {
            //problemo
            isomorphic = false
          }
        }
      }
    }

    isomorphic
  }
}