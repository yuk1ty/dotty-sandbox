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
  
  ignore("this will be made to deadlock") {
    import ch7.Par._
    val a = lazyUnit(42 + 1) // make fork
    val s = Executors.newFixedThreadPool(1)
    println(equal(s)(a, fork(a))) // have fork and create another fork on the same thread
  }
  
  test("testing extension methods of Nonblockng version") {
    import ch7.Nonblocking._
    val a = lazyUnit(1)
    val b = lazyUnit(2)
    val stringify: Int => String = _.toString
    
    // testing extension ops
    // call extension map
    a map stringify
    // call extension flatMap
    for {
      v1 <- a
      v2 <- b
    } yield v1 + v2 
  }
  
  test("this will not be made to deadlock") {
    import ch7.Nonblocking._
    val a = lazyUnit(42 + 1)
    val s = Executors.newFixedThreadPool(1)
    assert(equal(s)(a, fork(a)))
  }
}
