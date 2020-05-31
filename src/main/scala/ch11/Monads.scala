package ch11

import ch4.Option
import ch4.Option._
import ch7.Nonblocking
import ch7.Nonblocking._
  
given optionMonad as Monad[Option] {
  def unit[A](a: => A): Option[A] = Some(a)
  def [A, B](ma: Option[A]) flatMap (f: A => Option[B]): Option[B] = ma flatMap f
}
  
given listMonad as Monad[List] {
  def unit[A](a: => A): List[A] = List(a)
  def [A, B](l: List[A]) flatMap (f: A => List[B]): List[B] = l flatMap f
}
  
given parMonad as Monad[Par] {
  def unit[A](a: => A): Par[A] = Nonblocking.unit(a)
    // conflict naming space?
  def [A, B](p: Par[A]) flatMap (f: A => Par[B]): Par[B] = Nonblocking.flatMap(p)(f)
}
