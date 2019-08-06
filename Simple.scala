object MyModule {
  def abs(n : Int) : Int = {
    if (n<0) -n
    else n
  }

  def factorial(n: Int) : Int = {
    def go(n: Int,acc : Int) : Int = {
      if (n<=0) acc
      else go(n-1,n*acc)
    }

    go(n,1)
  }

  def fibo(n : Int) : Int = {
    if (n<=1) {
      return 1
    }
    if (n==2) {
      return 2
    }
    else fibo(n-1) + fibo(n-2)
  }

   def isSorted[A](as : Array[A], ordered : (A,A) => Boolean) : Boolean = {
     def itr(source : Int,dest : Int) : Boolean = {
       if (source < 0 || dest > as.length || source >= dest) {
         return true;
       }
       if (ordered(as(source),as(dest)) == false) {
         return false
       }

       return itr(source+1,dest) && itr(source,dest-1);

     }

     return itr(0,as.length-1);


   }

  def cmp(x : Int,y : Int) : Boolean = {
    return x <= y
  }

  def compose[A,B,C](f : B => C, g : A => B) : A => C = {
    def h(a : A) : C = {
      val b = g(a)
      f(b)
    }

    h
  }
  def main(args: Array[String]): Unit = {
    //println(factorial(10))
    //println(fibo(10))
    val arr : Array[Int] = Array(1,2,3,4,5,1,2,3)
    println(isSorted(arr,cmp))
  }
}

//object Hello extends App {
//  println(MyModule.)
//}