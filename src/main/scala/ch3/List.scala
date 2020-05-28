package ch3

enum List[+A] {
  case Cons[+A](head: A, tail: List[A]) extends List[A]
  case Nil
}

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1d
    case Cons(0d, _) => 0d
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))
  
  def setHead[A](xs: List[A], head: A): List[A] = xs match {
    case Nil => Cons(head, Nil)
    case Cons(_, t) => Cons(head, t)
  }
  
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Cons(h, t) => drop(t, n - 1)
      case Nil => Nil
    }
  }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  } 
  
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }
}
