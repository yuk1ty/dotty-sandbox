package ch6

import org.scalatest.funsuite.AnyFunSuite

class StateSpec extends AnyFunSuite {
  test("vending machine state machine") {
    enum Input {
      case Coin
      case Turn
    }
    
    case class Machine(locked: Boolean, candies: Int, coins: Int)
    
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      val modification = (i: Input) => (s: Machine) => {
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Input.Coin, Machine(false, _, _)) => s
          case (Input.Turn, Machine(true, _, _)) => s
          case (Input.Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + 1)
          case (Input.Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - 1, coin)
        }
      }
      
      for {
        _ <- State.sequence(inputs map (State.modify[Machine] _ compose modification))
        s <- State.get
      } yield (s.coins, s.candies)
    }
  }
}
