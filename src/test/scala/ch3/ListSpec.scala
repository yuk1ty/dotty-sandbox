package ch3

import org.scalatest.funsuite.AnyFunSuite
import ch3.List._

class ListSpec extends AnyFunSuite {
  
  test("create an object") {
    val listab: ch3.List[String] = Cons("a", Cons("b", Nil))
    listab match {
      case Cons(h, t) => {
        assert(h == "a")
        assert(t == Cons("b", Nil))
      }
      // ignore Nil matching case
    }
  }
  
  test("the sum of integer list (1, 2, 3) should be 6") {
    val intList = Cons(1, Cons(2, Cons(3, Nil)))
    assert(List.sum(intList) == 6)
  }
  
  test("the product of double list (1.0, 2.0, 3.0) should be 6") {
    val doubleList = Cons(1d, Cons(2d, Cons(3d, Nil)))
    assert(List.product(doubleList) == 6d)
  }
  
  test("given the sequence of string, generate List[String]") {
    val objective = List("a", "b", "c", "d")
    assert(objective == Cons("a", Cons("b", Cons("c", Cons("d", Nil)))))
  }
}
