package optional

enum Option[+A]:
  case Some(a: A)
  case None

  def map[B](f: A => B): Option[B] = this match
    case Some(x) => Some(f(x))
    case None => None

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(x) => f(x)
    case None => None
