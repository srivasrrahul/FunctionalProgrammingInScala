object Solution {
  //     def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
  //         val s1 = nums1.toSet
  //         val s2 = nums2.toSet

  //         val intersect = s1.intersect(s2).toArray
  //         intersect
  //     }
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    scala.util.Sorting.quickSort(nums1)
    scala.util.Sorting.quickSort(nums2)

    val intersect = new scala.collection.mutable.ArrayBuffer[Int]

    for (x <- nums1) {
      nums2.toSeq.search(x) match {
        case scala.collection.Searching.Found(_) => {
          intersect.toSeq.search(x) match {
            case scala.collection.Searching.Found(_) => {

            }
            case _ => {
              intersect.append(x)
            }
          }

        }
        case _ => {

        }
      }
    }


    intersect.toArray
  }
}