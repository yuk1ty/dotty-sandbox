package ch4

import org.scalatest.funsuite.AnyFunSuite

class OptionSpec extends AnyFunSuite {
  import ch4.Option
  import ch4.Option._
  
  test("map integer option to string option") {
    val option = Some(1)
    val actual = option.map(_.toString)
    assert(actual == Some("1"))
  }
  
  test("for-yield") {
    {
      // expect to pass all
      val actual = for {
        a <- Some(1)
        b <- Some(a + 2)
        c <- Some(b * 4)
      } yield c
      assert(actual == Some(12))
    }
    
    // expect to suspend
    {
      def returnNone: Option[Int] = None
      val actual = for {
        a <- Some(1)
        b <- returnNone
        c <- Some(b * 4) // not comes in
      } yield c
      assert(actual == None)
    }
  }
  
  test("filter by if the value is odd, expect to get None") {
    val option = Some(2)
    val actual = option.filter(_ % 2 != 0)
    assert(actual == None)
  }
  
  test("if option value is some then ignore given default value") {
    val option = Some(1)
    val actual = option.getOrElse(0)
    assert(actual == 1)
  }
  
  test("if option value is none then get specific default value") {    
    def returnNone(): Option[Int] = None
    val actual = returnNone().getOrElse(0)
    assert(actual == 0)
  }
  
  test("if option value is some then ignore given default value and return Option[Int]") {
    val option = Some(1)
    val actual =option.orElse(Some(0))
    assert(actual == Some(1))
  }

  test("if option value is none then get specific default value and return Option[Int]") {
    def returnNone(): Option[Int] = None
    val actual = returnNone().orElse(Some(0))
    assert(actual == Some(0))
  }
  
  test("concat two options with applying given function") {
    val a = Some(2)
    val b = Some(3)
    val actual = Option.map2(a, b)((a, b) => a * b)
    assert(actual == Some(6))
  }
  
  test("sequence") {
    import ch3.List._
    val objective = Cons(Some(1), Cons(Some(2), Nil))
    val actual = Option.sequence(objective)
    assert(actual == Some(Cons(1, Cons(2, Nil))))
  }
}
