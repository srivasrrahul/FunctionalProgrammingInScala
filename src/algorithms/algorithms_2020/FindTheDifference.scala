object Solution {
  def findTheDifference(s: String, t: String): Char = {
    val sortedS = s.toSeq.sorted.unwrap
    val sortedT = t.toSeq.sorted.unwrap
    var diffFound = false
    var diff = '0'
    // a h z
    // a b h z
    for (j <- 0 to sortedS.length-1 if diffFound == false) {
      if (sortedS(j) != sortedT(j)) {
        diffFound = true
        diff = sortedT(j)
      }
    }

    if (diffFound == false) {
      diffFound = true
      diff = sortedT.last
    }

    diff
  }
}