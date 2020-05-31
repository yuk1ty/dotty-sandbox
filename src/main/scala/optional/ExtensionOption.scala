package optional

// Rust-like implementation way
// Decouple implementation from type definition
object ExtensionOption {
  enum Option[+A] {
    case Some(a: A)
    case None
  }
  
  extension impl {
    def [A, B](o: Option[A]) map (f: A => B): Option[B] = {
      import ExtensionOption.Option._
      o match {
        case Some(a) => Some(f(a))
        case None => None
      }
    }
    
    def [A, B](o: Option[A]) flatMap (f: A => Option[B]): Option[B] = {
      import ExtensionOption.Option._
      o match {
        case Some(a) => f(a)
        case None => None
      }
    }
  }
}
