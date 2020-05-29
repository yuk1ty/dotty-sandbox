package ch3

import org.scalatest.funsuite.AnyFunSuite
import ch3.Tree._

class TreeSpec extends AnyFunSuite {
  test("tree size") {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    assert(Tree.size(tree) == 5)
  }
  
  test("given the integers then map to string values") {
    val tree = Branch(Leaf(1), Leaf(2))
    val actual = Tree.map(tree)(_.toString())
    assert(actual == Branch(Leaf("1"), Leaf("2")))
  }
}
