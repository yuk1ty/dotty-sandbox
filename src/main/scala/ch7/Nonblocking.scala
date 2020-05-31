package ch7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {
  sealed trait Future[+A] {
    private[ch7] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val latch = new CountDownLatch(1)
    // first it calls p#apply and second it calls Future#apply
    p(es) { a =>
      ref.set(a); latch.countDown()
    }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    es =>
      new Future[B] {
        override def apply(k: B => Unit): Unit =
          pa(es)(a => eval(es) { k(f(a)) })
    }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es =>
      new Future[B] {
        override def apply(k: B => Unit): Unit = pa(es)(a => f(a)(es)(k))
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })
  
  def equal[A](e: ExecutorService)(a: Par[A], b: Par[A]): Boolean =
    run(e)(a) == run(e)(b)
  
  extension ops {
    def [A, B](o: Par[A]) map (f: A => B): Par[B] = Nonblocking.map(o)(f)
    def [A, B](o: Par[A]) flatMap (f: A => Par[B]): Par[B] = Nonblocking.flatMap(o)(f)
  }
}
