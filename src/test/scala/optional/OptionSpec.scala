package optional

import optional.Option._
import org.scalatest.funsuite.AnyFunSuite

class OptionSpec extends AnyFunSuite {
  test("integer option map to string") {
    val option = Some(1)
    val actual = option.map(_.toString)
    assert(actual == Some("1"))
  }
}
