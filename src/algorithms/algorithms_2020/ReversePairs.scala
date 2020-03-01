import scala.collection.Searching


object Solution {

  def print_content(arr : Array[Int],j : Int,m : Int,k : Int): Unit = {
    print(" ------> ")
    for (i <- j to m) {
      print(arr(i) + ",")
    }

    print(" | ")
    for (i <- m+1 to k) {
      print(arr(i) + ",")
    }


    print(" <------ ")
    println()

  }

  def reversePairs(nums: Array[Int]): Int = {

    var arr = Array.ofDim[Long](nums.length)
    for (i <- 0 to arr.length-1) {
      arr(i) = nums(i)
    }

    var reverse_pairs_count = 0

    def merge(low : Int,mid : Int,high : Int) : Unit = {
      //println("merging " + low + " " + mid + " " + high)
      //print("unsorted : ")
      //print_content(arr,low,mid,high)
      //print_content(arr,mid+1,high)
      var j = low
      var k = mid+1
      var i = 0

      val right_size = high-(mid+1) + 1

      for (r <- mid+1 to high) {
        //println(" in index " + r + " ")
        val l : Long = arr(r)
        val reverse_pair_min : Long  = 2*l + 1
        //println("reverse_pair_min " + reverse_pair_min)
        //println("in index " + r + " " + "searchinh for " + reverse_pair_min)


        arr.search(reverse_pair_min,low,mid+1) match {
          case Searching.Found(found_index) => {
            //println("Incrmenting by p0 " + (mid-found_index+1))
            reverse_pairs_count = reverse_pairs_count + (mid-found_index+1)
          }
          case Searching.InsertionPoint(insertion_point) =>  {
            //println("INsertio poin is " + insertion_point)
            reverse_pairs_count = reverse_pairs_count + (mid-insertion_point+1)

          }
        }
      }

      //Standard merge
      val temp_array = Array.ofDim[Long](high-low+1)

      while (j <= mid && k <= high) {
        if (arr(j) > arr(k)) {
          temp_array(i) = arr(k)
          i = i + 1
          k = k + 1

        }else {
          if (arr(j) <= arr(k)) {
            temp_array(i) = arr(j)
            i = i + 1
            j = j + 1
          }
        }

      }


      while (k <= high) {
        temp_array(i) = arr(k)
        i = i + 1
        k = k + 1
      }

      while (j<= mid) {
        temp_array(i) = arr(j)
        i = i + 1
        j = j + 1
      }

      Array.copy(temp_array,0,arr,low,(high-low+1))

      //print("sorted : ")
      //print_content(arr,low,mid,high)



    }

    def sort(low : Int,high : Int) : Unit = {
      if (high > low) {
        val mid = low + (high-low)/2
        sort(low,mid)
        sort(mid+1,high)
        merge(low,mid,high)
      }
    }


    sort(0,arr.length-1)
    //println(" r " + reverse_pairs_count)
    reverse_pairs_count
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(2147483647,2147483647,2147483647,2147483647,2147483647,2147483647)
    println(reversePairs(arr))
    println(arr.mkString(","))
  }
}