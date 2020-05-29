package ch3

import ch3.List._

enum Option[+A] {
  case Some(a: A)
  case None
  
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case None => None
  } 
  
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }
  
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
  
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  
  def orElse[B >: A](op: => Option[B]): Option[B] = 
    this map (Some(_)) getOrElse op
}

object Option {
  def map2[A, B, C](a: Option[A], b: Option[B])
                   (f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))
  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case Cons(h, t) => h flatMap (hh => sequence(t) map (Cons(hh, _)))
  }
}