package ch3

enum Tree[+A] {
  case Leaf[A](value: A) extends Tree[A]
  case Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
}

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }
  
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(l) => l
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(l) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
}
