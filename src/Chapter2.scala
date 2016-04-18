import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

/**
 * Created by Rahul on 11/7/15.
 */
//Functional programming in scala solved problems

object MyModule  {
  def abs(n : Int) = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(n : Int) = {
    val msg = "The abs value is %d"
    msg.format(abs(n))
  }

  def factorial(n : Int) : Int = {
    if (n <= 1) 1
    else n * factorial(n-1)
  }



  def factorialTail(n : Int) : Int =  {
    @annotation.tailrec
    def factorialItr(n: Int, result: Int): Int = {
      if (n <= 0) result
      else factorialItr(n - 1, result * n)
    }

    factorialItr(n,1)
  }

  def formatResult(name : String,source : Int,fun : Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(name,source,fun(source))
  }

  def findFirst(ss : Array[String], key : String) :Int = {
    @annotation.tailrec
    def loop (n : Int) : Int = {
      if (n >= ss.length) -1
      else if (key == ss(n)) n
      else loop(n+1)
    }

    loop(0)
  }

  def findFirstGeneric[T](ss : Array[T],checker : T => Boolean) : Int = {
    @annotation.tailrec
    def loop (n : Int) : Int = {
      if (n >= ss.length) -1
      else if (checker(ss(n))) n
      else loop(n+1)
    }

    loop(0)
  }

  def isSorted[T](as : Array[T],ordered : (T,T) => Boolean) : Boolean = {
    @annotation.tailrec
    def loop(n : Int) : Boolean = {
      if (n >= as.length-1) true
      else if (ordered(as(n),as(n+1)) == false) false
      else loop(n+1)
    }

    loop(0)
  }

  def partial1[A,B,C](a : A,f : (A,B) => C) : B => C = {
    (b : B) => f(a,b)
  }

  def curry[A,B,C](f: (A,B) => C ) : A => (B => C) = {
    (a : A) => (b : B) => f(a,b)
  }

  def uncurry[A,B,C](f : A => B => C) : (A,B) => C = {
    (a : A,b : B) => {
      val g = f(a)
      g(b)
    }
  }

  def compose[A,B,C](f: B => C,g : A => B) : A => C = {
    (a : A) => {
      f(g(a))
    }
  }






  def main(args : Array[String]) : Unit = {
    //println(formatAbs(-42))
    //println(formatResult("factorial",10,factorialTail))
    //val arr = Array("hello","world","test")
    //println(findFirstGeneric(arr, (x : String) => x == "test"))

    val arr = Array(1,2,3,4,6,5)
    println(isSorted(arr,(x : Int,y : Int)  => x <= y))
  }
}

