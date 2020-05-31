package ch7

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)

  private final case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(eventIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
  
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
  
  def map[A, B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))
  
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)
  
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List()))((h, t) => map2(h, t)(_ :: _))
  
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }
  
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val fbs = as.map(asyncF(a => if f(a) then List(a) else List.empty))
    map(sequence(fbs))(_.flatten)
  }
  
  def equal[A](e: ExecutorService)(a: Par[A], b: Par[A]): Boolean =
    a(e).get == b(e).get
}
