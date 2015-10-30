package oscar.algebra.test

import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, FunSuite}
import oscar.algebra.Interval

@RunWith(classOf[JUnitRunner])
class IntervalTester extends FunSuite with Matchers {

  test("[-Inf, 0] contains -5") {
    Interval(-Double.MaxValue, true, 0.0, true).contains(-5) should be (true)
  }

  test("[-Inf, 0] contains 0") {
    Interval(-Double.MaxValue, true, 0.0, true).contains(0) should be (true)
  }

  test("[-Inf, 0[ contains 0") {
    Interval(-Double.MaxValue, true, 0.0, false).contains(0) should be (false)
  }

  test("[-Inf, 0] contains 5") {
    Interval(-Double.MaxValue, true, 0.0, true).contains(5) should be (false)
  }

  test("[0, 5] contains -5") {
    Interval(0.0, true, 5.0, true).contains(-5) should be (false)
  }

  test("[0, 5] contains 5") {
    Interval(0.0, true, 5.0, true).contains(5) should be (true)
  }

  test("[0, 5] contains 0") {
    Interval(0.0, true, 5.0, true).contains(0) should be (true)
  }

  test("]0, 5[ contains 5") {
    Interval(0.0, false, 5.0, false).contains(5) should be (false)
  }

  test("]0, 5[ contains 0") {
    Interval(0.0, false, 5.0, false).contains(0) should be (false)
  }

  test("[-5, 0] intersect ]0, 5]") {
    Interval(-5.0, true, 0.0, true).intersect(
      Interval(0.0, false, 5.0, true)
    ) should be (false)
  }

  test("[-5, 0] intersect [0, 5]") {
    Interval(-5.0, true, 0.0, true).intersect(
      Interval(0.0, true, 5.0, true)
    ) should be (true)
  }

  test("[-5, 0[ intersect [0, 5]") {
    Interval(-5.0, true, 0.0, false).intersect(
      Interval(0.0, true, 5.0, true)
    ) should be (false)
  }

  test("[-5, 1] intersect [0, 5]") {
    Interval(-5.0, true, 1.0, true).intersect(
      Interval(0.0, true, 5.0, true)
    ) should be (true)
  }

  test("[-5, 0] intersect [-1, 5]") {
    Interval(-5.0, true, 0.0, true).intersect(
      Interval(-1.0, true, 5.0, true)
    ) should be (true)
  }

  test("[5, 10] intersect [0, 5]") {
    Interval(5.0, true, 10.0, true).intersect(
      Interval(0.0, true, 5.0, true)
    ) should be (true)
  }

  test("[5, 10] intersect [0, 5[") {
    Interval(5.0, true, 10.0, true).intersect(
      Interval(0.0, true, 5.0, false)
    ) should be (false)
  }

  test("]5, 10] intersect [0, 5]") {
    Interval(5.0, false, 10.0, true).intersect(
      Interval(0.0, true, 5.0, true)
    ) should be (false)
  }

  test("[5, 10] intersect [5, 10]") {
    Interval(5.0, true, 10.0, true).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("]5, 10[ intersect [5, 10]") {
    Interval(5.0, false, 10.0, false).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("]5, 11[ intersect [5, 10]") {
    Interval(5.0, false, 11.0, false).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("]4, 10[ intersect [5, 10]") {
    Interval(4.0, false, 10.0, false).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("[6, 8] intersect [5, 10]") {
    Interval(6.0, true, 8.0, true).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("]6, 8] intersect [5, 10]") {
    Interval(6.0, false, 8.0, true).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("[6, 8[ intersect [5, 10]") {
    Interval(6.0, true, 8.0, false).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("]6, 8[ intersect [5, 10]") {
    Interval(6.0, false, 8.0, false).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (true)
  }

  test("[6, 8] intersect ]5, 10]") {
    Interval(6.0, true, 8.0, true).intersect(
      Interval(5.0, false, 10.0, true)
    ) should be (true)
  }

  test("[6, 8] intersect [5, 10[") {
    Interval(6.0, true, 8.0, true).intersect(
      Interval(5.0, true, 10.0, false)
    ) should be (true)
  }

  test("[6, 8] intersect ]5, 10[") {
    Interval(6.0, true, 8.0, true).intersect(
      Interval(5.0, false, 10.0, false)
    ) should be (true)
  }

  test("[0, 3] intersect [5, 10]") {
    Interval(0.0, true, 3.0, true).intersect(
      Interval(5.0, true, 10.0, true)
    ) should be (false)
  }
}
