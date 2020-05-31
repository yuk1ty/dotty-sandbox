package ch7

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.funsuite.AnyFunSuite

class ParSpec extends AnyFunSuite {
  test("sort parlist") {
    val ec: ExecutorService = Executors.newFixedThreadPool(1)
    val sorted = Par.sortPar(Par.unit(List(1, 3, 5, 2, 4, 6)))
    assert(Par.run(ec)(sorted) == Par.run(ec)(Par.unit(List(1, 2, 3, 4, 5, 6))))
  }
  
  test("checking some laws") {
    import ch7.Par._
    val ec: ExecutorService = Executors.newFixedThreadPool(1)
    
    val a = 1
    val b = unit(a)
    val f: Int => String = _.toString
    
    // initial law
    assert(equal(ec)(map(unit(a))(f), unit(f(a))))
    
    // substitute identity function for f
    assert(equal(ec)(map(unit(a))(identity), unit(identity(a))))
    
    // simplify
    assert(equal(ec)(map(unit(a))(identity), unit(a)))
    
    // substitute b for unit a on both sides
    assert(equal(ec)(map(b)(identity), b))
  }
}
