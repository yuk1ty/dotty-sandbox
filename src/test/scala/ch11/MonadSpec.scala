package ch11

import org.scalatest.funsuite.AnyFunSuite

class MonadSpec extends AnyFunSuite {
  
  import ch11.Monad
  import ch4.Option
  
  test("summon Option Monad") {
    val option = summon[Monad[Option]]
  }
}
