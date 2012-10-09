package oscar.stochastic

import oscar.stochastic._
import scala.collection.immutable._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

@RunWith(classOf[JUnitRunner])
class TableFunctionTest extends FunSuite with ShouldMatchers {
  implicit def intOp = IntOp

  test("test1") {
    val t1 = TableFunction(SortedMap(1 -> 2, 2 -> 3))
    val t2 = TableFunction(SortedMap(1 -> 3, 2 -> 5))

    val sum = t1 + t2

    sum(1) should equal(5)
    sum(2) should equal(8)

    for (t <- 3 to 1000) {
      sum(t) should equal(0)
    }
  }
  test("testAdd") {
    val t1 = TableFunction(SortedMap(1 -> 2, 2 -> 3))
    val t2 = TableFunction(SortedMap(1 -> 3, 2 -> 5))

    t1.add(t2)

    t1(1) should equal(5)
    t1(2) should equal(8)

    for (t <- 3 to 1000) {
      t1(t) should equal(0)
    }
  }

  test("testMap") {
    val t1 = TableFunction(SortedMap(1 -> 2, 2 -> 3))
    val t2 = TableFunction(SortedMap(1 -> 3, 2 -> 5))

    t1.add(t2)

    val m = t1.map( (t,v) => (t,2*v)) //for (((t, v)) <- t1) yield { (t, 2 * v) }
    m(1) should equal(10)
    println(m);
  }

  test("testMax") {
    val t1 = TableFunction(SortedMap(1 -> 5, 2 -> 3))
    val t2 = TableFunction(SortedMap(1 -> 3, 2 -> 5))
    val t3 = TableFunction(SortedMap(1 -> 5, 2 -> 5))

    val max = t1 max t2

    for ( t <- -100 to 100 )
      max(t) should equal(t3(t))
    
  }

  test("delay"){
    
    val t1 = TableFunction(SortedMap(1 -> 5, 2 -> 3, 5-> 10))
    val tres = TableFunction(SortedMap(3 -> 5, 4 -> 3, 7-> 10))
    val delayed = t1 delay(2)
    
    delayed should equal(tres)
  }
  
}