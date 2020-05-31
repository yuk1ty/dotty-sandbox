package ch11

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def [A, B](ma: F[A]) flatMap (f: A => F[B]): F[B]
  def [A, B](ma: F[A]) map (f: A => B): F[B] = 
    ma flatMap(f andThen unit) 
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    ma flatMap(a => mb map(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List.empty))((x, acc) => map2(x, acc)(_ :: _))
  def traverse[A, B](lma: List[A])(f: A => F[B]): F[List[B]] =
    lma.foldRight(unit(List.empty))((x, acc) => map2(f(x), acc)(_ :: _))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
}
