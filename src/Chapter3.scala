/**
 * Created by Rahul on 3/6/16.
 */


import math.Numeric.Implicits._
import Numeric.Implicits._
import Ordering.Implicits._
import scala.Int.MinValue

object ListOperations {
  def sum(ints : List[Int]) : Int = ints match {
    case Nil => 0
    case x::xs => x + sum(xs)
  }

  def product(ints : List[Int]) : Int = ints match {
    case Nil => 1
    case x::xs => x * product(xs)
  }

  def tail[T](lst : List[T]) : List[T] = lst match {
    case Nil => Nil
    case x::xs => xs
  }

  def setHead[T](value : T, lst : List[T]) : List[T] = {
    value :: tail(lst)
  }

  def drop[T](n : Int,lst : List[T]) : List[T] = {
    n match {
      case 0 => lst
      case _ => lst match {
        case Nil => Nil
        case x::xs => drop(n-1,xs)
      }
    }
  }

  def dropWhile[T](lst : List[T], checker : T => Boolean) : List[T] = {
    lst match {
      case Nil => Nil
      case x::xs =>
        if (checker(x)) dropWhile(xs,checker)
        else x::dropWhile(xs,checker)
    }
  }

  def init[T](lst : List[T]) : List[T] = {
    lst match {
      case Nil => Nil
      case xs::Nil => Nil
      case x::xs => x::init(xs)

    }
  }

  def append[T](lst1 : List[T], lst2 : List[T]) : List[T] = {
    lst1 match {
      case Nil => lst2
      case x::xs => x :: append(xs,lst2)
    }
  }

  def foldRight[A,B](as : List[A],default : B) (f : (A,B) => B) : B = {
    as match {
      case Nil => default
      case x::xs => f(x,foldRight(xs,default)(f))
    }
  }

  def foldLeft[A,B] (lst : List[A],default : B) (f : (B,A) => B) : B = {
    lst match {
      case Nil => default
      case x::xs => foldLeft(xs,f(default,x))(f)
    }
  }

  def sum2(ns : List[Int]) : Int = {
    foldRight(ns,0)((x,y) => x+y)
  }

  def length2[T] (lst : List[T]) : Int = {
    foldRight(lst,0)((x,y) => 1+y)
  }

  def sum3(ns : List[Int]) : Int = {
    foldLeft(ns,0)((x,y) => x+y)
  }

  def product3(ns : List[Int]) : Int = {
    foldLeft(ns,1) ((x,y) => x*y)
  }

  def length3(lst : List[Int]) : Int = {
    foldLeft(lst,0) ((x,y) => 1+x)
  }

  def reverse[T](lst : List[T]) : List[T] = {
    lst match {
      case Nil => Nil
      case x::xs => append(reverse(xs),List(x))
    }
  }

  def reverseWithFold[T](lst : List[T]) : List[T] = {
    foldLeft(lst,List[T]())((x,y) => y :: x)


  }

//  def foldRightWithFoldLeft[T1,T2](lst : List[T1],default : T2) (f : (T1,T2) => T2) : T2 = {
//    //foldLeft(lst,)
//  }


  def appendWithFold[T](lst1 : List[T],lst2 : List[T]) : List[T]= {
    foldRight(lst1,lst2)((x,y) => x :: y)
  }


  def concat[T](lst : List[List[T]]) : List[T]= {
    lst match {
      case Nil => Nil
      case x :: xs => append(x, concat(xs))
    }
  }

  //Section 3.16
  def transform(lst : List[Int]) : List[Int] = {
    lst match {
      case Nil => Nil
      case x::xs => (x+1) :: xs
    }
  }

  def turnToString(lst : List[Double]) : List[String] = {
    lst match {
      case Nil => Nil
      case x::xs => x.toString() :: turnToString(xs)
    }
  }

  def mymap[A,B](lst : List[A])(f : A => B) : List[B]= {
    lst match {
      case Nil => Nil
      case x::xs => f(x) :: mymap(xs)(f)
    }
  }

  def myfilter[A](lst : List[A])(f : A => Boolean) : List[A]= {
    lst match {
      case Nil => Nil
      case x::xs => if (f(x)) {
        x::myfilter(xs)(f)
      }else {
        myfilter(xs)(f)
      }
    }
  }

  def flatmap[A,B](lst : List[A])(f: A => List[B]) : List[B] = {
    lst match {
      case Nil => Nil
      case x::xs => append(f(x),flatmap(xs)(f))
    }
  }

  def filterUsingFlatMap[T](lst : List[T]) (f: T => Boolean) : List[T] = {
    flatmap(lst)((x) => if (f(x)) {
      List(x)
    }else {
      List()
    })
  }


  //Assume both of same lenth
  def adder(lst1 : List[Int],lst2 : List[Int]) : List[Int] = {
    lst1 match {
      case Nil => Nil
      case x::xs => lst2 match {
        case Nil => Nil
        case y::ys => (x+y) :: adder(xs,ys)
      }
    }
  }

  def zipWith[T](lst1 : List[T],lst2 : List[T])(f : (T,T) => T) : List[T] = {
    lst1 match {
      case Nil => Nil
      case x::xs => lst2 match {
        case Nil => Nil
        case y::ys => f(x,y) :: zipWith(xs,ys)(f)
      }
    }
  }

  def adderWithZipWith(lst1 : List[Int],lst2 : List[Int]) : List[Int] = {
    zipWith(lst1,lst2)((x,y) => x+y)
  }


  def hasSubsequence[T](seq : List[T], subseq : List[T]) : Boolean = {
    subseq match {
      case Nil => true
      case y::ys => seq match {
        case Nil => false
        case x::xs => if (x == y) {
          hasSubsequence(xs,ys) || hasSubsequence(xs,subseq)
        }else {
          hasSubsequence(xs,subseq)
        }
      }
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A] (value : A) extends Tree[A]
  case class Branch[A] (left : Tree[A], right : Tree[A]) extends Tree[A]

  def countNodes[T](node : Tree[T]) : Int = {
    node match {
      case Leaf(_) => 1
      case Branch(left,right) => 1+countNodes(left) + countNodes(right)
    }
  }

  def maxNode[T](node : Tree[T]) : Tree[T] = {
    node match {
      case Leaf(x) => Leaf(x)
      case Branch(left ,right ) =>
//        val left  = maxNode(left)
//        val right  = maxNode(right)
//        if (left.## < right.##) {
//          left
//        }else {
//          right
//        }
        //Non optimal calling maxNode twice
        val x : Tree[T] = maxNode(left)
        val y : Tree[T] = maxNode(right)
        if (x.## < y.##) {
          y
        }else {
          x
        }
//        if (maxNode(left).## < maxNode(right).##) {
//          maxNode(right)
//        }else {
//          maxNode(left)
//        }

    }


  }

  def depth[T](node : Tree[T]) : Int = {
    node match {
      case Leaf(v) => 0
      case Branch(left,right) =>
        val x : Int = depth(left)
        val y : Int = depth(right)
        if (x.## < y.##) {
          y+1
        }else {
          x+1
        }
    }
  }

  def map[T](node : Tree[T])(f: Tree[T] => Tree[T]) : Tree[T] = {
    node match {
      case Leaf(v) => f(Leaf(v))
      case Branch(left,right) =>
        Branch(f(left),f(right))
    }
  }

//  def fold[A,B](node : Tree[A],default : B)(f: (Tree[A],B) => B) = {
//    node match {
//      case Leaf(v) => default
//      case Branch(left,right) =>
//        f(left,)
//    }
//  }



  def main(args : Array[String]): Unit = {
    var lst = List(1,2,3,4,100)
    var lst1 = List(100)
    //println(append(lst,lst1))
    //println(sum2(lst))
    //println(length3(lst))
    //println(reverseWithFold(lst))
    //println(concat(List(List(1,2),List(3,4))))
    //println(appendWithFold(List(1,2),List(3,4)))
    //println(transform(List(1,2,3,4)))
    //println(myfilter(lst)((x) => x%2 == 0))
    //println(flatmap(lst)((x) => List(x,x)))
    //println(filterUsingFlatMap(lst)((x) => x%2 == 0))
    //println(hasSubsequence(lst,lst1))
    val l1 = new Leaf(10)
    val l2 = new Leaf(20)
    val node = new Branch(new Leaf(10),new Leaf(20))
    val node1 = new Branch(new Leaf(30),new Leaf(40))
    val node2 = new Branch(node,node1)
    println(depth(node2))

    //println(product(lst))
    //println(tail(lst))
    //println(setHead(5,lst))
    //println(drop(2,lst))
    //println(dropWhile(lst,(x : Int) => x % 2 == 0))
    //println(init(lst))
  }
}

