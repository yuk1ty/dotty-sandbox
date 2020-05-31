package ch11

trait Functor[F[_]] {
  def [A, B](fa: F[A]) map (f: A => B): F[B]
}
