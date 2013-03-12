package oscar.stochastic

import oscar.stochastic._
import oscar.stochastic
import scala.collection.immutable._
import scala.collection._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalatest.prop.Checkers
import org.scalacheck.Test._

@RunWith(classOf[JUnitRunner])
class TableFunctionTest extends FunSuite with ShouldMatchers with Checkers {

  implicit def op = IntOp
  implicit def tfGen = org.scalacheck.Gen.resultOf { m: immutable.Map[Int, Int] => TableFunction(m) }

  test("equals") {
    check {
      forAll { m1: immutable.Map[Int, Int] =>
        TableFunction(m1) should not equal (TableFunction(m1))
        true
      }
    }
  }

  test("Sum") {
    check {
      forAll(tfGen, tfGen) { (m1: TableFunction[Int], m2: TableFunction[Int]) =>
          def tSum = {
            val mSum = mutable.Map[Int, Int]().withDefaultValue(0)
            for ((k, v) <- m1) mSum(k) = v
            for ((k, v) <- m2) mSum(k) = mSum(k) + v
            mSum
          }
        TableFunction(tSum) == TableFunction(TableFunction(m1) + TableFunction(m2))
      }
    }
  }

  test("Max") {
    check {
      forAll(tfGen, tfGen) { (m1: TableFunction[Int], m2: TableFunction[Int]) =>
          def tMax = {
            val mMax = mutable.Map[Int, Int]().withDefaultValue(0)
            for ((k, v) <- m1) mMax(k) = scala.math.max(0, v)
            for ((k, v) <- m2) mMax(k) =
              if (m1.isInDomain(k)) scala.math.max(m1(k), v)
              else scala.math.max(mMax(k), v)
            mMax
          }

        TableFunction(tMax) should equal(TableFunction(TableFunction(m1) max TableFunction(m2)))
        true
      }
    }
  }
  test("multiply") {
    check {
      forAll { (m: immutable.Map[Int, Int], factor: Int) =>
        val mM = mutable.Map[Int, Int]()
        for ((k, v) <- m) mM(k) = factor * v
        TableFunction(mM) should equal(TableFunction(TableFunction(m) * factor))
        true
      }
    }
  }
  test("testAdd") {

    check {
      forAll(tfGen, tfGen) { (m1: TableFunction[Int], m2: TableFunction[Int]) =>
          def tSum = {
            val mSum = mutable.Map[Int, Int]().withDefaultValue(0)
            for ((k, v) <- m1) mSum(k) = v
            for ((k, v) <- m2) mSum(k) = mSum(k) + v
            mSum
          }
        val t1 = TableFunction(m1)
        val t2 = TableFunction(m2)
        t1.add(t2)
        TableFunction(tSum) == TableFunction(t1)
      }
    }
  }

  test("testMap") {
    check {
      forAll { (m1: immutable.Map[Int, Int], f: Int => Int) =>
        TableFunction(m1.map { case (a, b) => (a, f(b)) }) == TableFunction(TableFunction(m1).map { b: Int => f(b) })
      }
    }
  }

  test("delay") {
    check {
      forAll { (m1: immutable.Map[Int, Int], d: Int) =>
        val md = mutable.Map[Int, Int]()
        for ((k, v) <- m1) md(k + d) = v
        TableFunction(md) should equal(TableFunction(TableFunction(m1).delay(d)))
        true
      }
    }
  }
  test("ApplyView.map") {
    check {
      forAll { (m: immutable.Map[Int, Int], f: Int => Int, g: Int => Int) =>
        TableFunction(m).map(f).map(g) should equal(TableFunction(m.map { case (k, v) => (k, (g(f(v)))) }))
        true
      }
    }
  }
  test("TableFunction.sum") {
    check {
      forAll { (list: List[immutable.Map[Int, Int]]) =>
        val mSum = mutable.Map[Int, Int]().withDefaultValue(0)
        for (t <- list; (k, v) <- t) mSum(k) += v
        TableFunction(mSum) should equal(TableFunction.sum(list.map { TableFunction(_) }))
        true
      }
    }
  }

}