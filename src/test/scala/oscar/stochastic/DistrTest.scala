package oscar.stochastic

import oscar.invariants._
import oscar.stochastic._
import scala.util.continuations._
import scala.util.continuations.suspendable
import scala.collection.immutable._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec

class Solver extends StochasticSolver[Unit]

class DistrTest extends FunSuite with ShouldMatchers {

  implicit def intOp = IntOp
  implicit def doubleOp = DoubleOp
  implicit val m = new Solver

  //implicit def cont2Fix[A](c: A @suspendable) = reset(c)

  test("Flip") {
    reset[Unit,Unit]{
      val flip = new Flip(0.15)
      var sum = 0
      (0 to 100000).suspendable.foreach[Unit]{_=> sum += (if (flip.apply[Unit](m)) 1 else 0)}
      val a = assert(sum > 14600 && sum < 15400)
    }
  }

  test("LearnedNumerical") {
    val n = new LearnedNumerical[Double]

    n() = 4
    n() += 5

    n() should equal(9)

    n observe

    n() = 9

    n observe

    n.squaredTot should equal(162)
    n.tot should equal(18)
  }

  test("aggregation of two LearnedNumerical") {
    val m = new LearnedNumerical[Double]
    val n = new LearnedNumerical[Double]

    m observe (5)
    m observe (10)

    n.observe(2)
    n.observe(9)

    m aggregate (n)

    m.squaredTot should equal(210)
    m.tot should equal(26)
  }

  test("equality of two LearnedNumerical") {
    val m = new LearnedNumerical[Double]
    val n = new LearnedNumerical[Double]

    m.observe(5)
    m.observe(6)
    m observe (10)

    n.observe(10)
    n.observe(5)
    n.observe(6)

    m should equal(n)
  }
  test("equality of two LearnedNumerical (1)") {
    val m = new LearnedNumerical[Double]
    val n = new LearnedNumerical[Double]

    m.observe(5)
    m.observe(6)
    m observe (10)

    n.observe(10)
    n.observe(5)
    n.observe(6)

    m should equal(n)
  }

  test("non-equality of two LearnedNumerical (2)") {
    val m = new LearnedNumerical[Double]
    val n = new LearnedNumerical[Double]

    m.observe(5)
    m.observe(7)
    m observe (10)

    n.observe(10)
    n.observe(6)
    n.observe(6)

    m should not equal (n)
  }

  test("quantiles") {

    val n = AbstractLearnedQuantiles[Double](0.0, 0.0)

    for (i <- 1 to 10) {
      n.observe(i)
    }

    n quantileUp (0.9, 10) should equal(9)
    n quantileUp (0.95, 10) should equal(10)
    n quantileUp (0.0001, 10) should equal(1)

    n quantileDown (0.75, 10) should equal(7)

    n quantileUp (0.9, 20) should equal(8)
  }

  test("LearnedNumerical Function") {
    val f = new LearnedNumericalFunctionWithMean[Double]

    val obs1 = Map(2 -> 7.0, 5 -> 10.0, 6 -> (-100.0), 50 -> 3.0)
    val obs2 = Map(2 -> 9.0, 3 -> 200.0, 6 -> 12.0, 100 -> 10.0)

    for ((t, v) <- obs1) f(t) = v
    f observe

    for ((t, v) <- obs2) f(t) = v
    f observe

    verify(f)

    val g = new LearnedNumericalFunctionWithMean[Double]
    g observe (obs1)
    g observe (obs2)

    verify(g)

    val h = new LearnedNumericalFunctionWithMean[Double]
    val o1 = TableFunction(obs1)
    val o2 = TableFunction(obs2)
    h observe (o1)
    h observe (o2)

    verify(h)
  }
  def verify(f: LearnedNumericalFunctionWithMean[Double]) {
    f mean (2) should equal(8)
    f mean (3) should equal(100)
    f mean (6) should equal(-44)
    f mean (7) should equal(0)
    f mean (100) should equal(5)
    f mean (50) should equal(1.5)
    
    intercept[ArrayIndexOutOfBoundsException] {
      f(-1) should equal(0)
    }
  }

  test("choices") {
    val choice = new NumericalChoice(List((0.2, 1), (0.3, 2), (0.5, 8)))
    reset[Unit,Unit]{
      var sum = 0.0
      iter2susp[Int](0 to 100000).suspendable.foreach[Unit]{_ => sum += choice.apply[Unit](m)}

//      val a = assert(sum < 500000)
//      val b = assert(sum > 460000)
//
//      choice.mean should equal(4.8)
//      choice.min should equal(1)
//      choice.max should equal(8)
//
//      val c = intercept[IllegalArgumentException] {
//        val choice2 = new Choice(List((0.2, 1), (0.3, 2), (0.3, 8)))
//      }
    }
  }
}