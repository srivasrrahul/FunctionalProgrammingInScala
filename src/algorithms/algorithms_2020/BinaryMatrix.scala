
trait BinaryMatrix {
  def get(row: Int, col: Int): Int
  def dimensions(): Array[Int]
}


object Solution {
  def leftMostColumnWithOne(binaryMatrix: BinaryMatrix): Int = {
    val dim = binaryMatrix.dimensions()
    val rows = dim(0)
    val cols = dim(1)
    def findLeftMostOne(rowId : Int) : Option[Int] = {
      def iter(left : Int,right: Int) : Option[Int] = {
        if (left == right) {
          if (binaryMatrix.get(rowId,left) == 1) {
            Some(left)
          }else {
            None
          }
        }else {
          if (left > right) {
            None
          }else {
            val mid = left + (right - left)/2
            if (binaryMatrix.get(rowId,mid) == 1)  {
              iter(left,mid-1) match {
                case None => Some(mid)
                case Some(left) => Some(left)
              }
            }else {
              iter(mid+1,right)
            }
          }
        }
      }

      iter(0,cols-1)
    }

    var leftMostCol : Option[Int] = None
    for (j <- 0 to rows-1) {
      findLeftMostOne(j) match {
        case None => {

        }
        case Some(index) => {
          //println(index)
          leftMostCol match {
            case Some(prevIndex) => {
              if (index < prevIndex) {
                leftMostCol = Some(index)
              }
            }
            case _ => {
              leftMostCol = Some(index)
            }
          }
        }
      }
    }

    leftMostCol match {
      case None => -1
      case Some(colIndex) => colIndex
    }
  }


}