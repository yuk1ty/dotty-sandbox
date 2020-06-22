package ch10

given stringMonoid as Monoid[String] {
  def op(a1: String, a2: String): String = a1 + a2
  val zero: String = ""
}

given listMonoid[A] as Monoid[List[A]] {
  def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
  val zero: List[A] = Nil
}
