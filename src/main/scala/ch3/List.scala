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
}
