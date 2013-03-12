package oscar.stochastic

import oscar.stochastic._
import scala.collection.immutable._
import scala.math.abs
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
import java.util.Arrays

@RunWith(classOf[JUnitRunner])
class LearnTest extends FunSuite with ShouldMatchers with Checkers with Learning {

  object Dop extends Operator[Double] {
    def zero = 0.0

    override def compare(a: Double, b: Double) = {
      val minUlp = scala.math.min(scala.math.ulp(a), scala.math.ulp(b))
      if (abs(a - b) <= minUlp) 0 //(a==b || (new java.math.BigDecimal(abs(a - b))).precision > 10) 0
      else if (a < b) -1
      else +1
    }
  }
  implicit def d: Operator[Double] = DoubleOp

  def lNumber(list: TraversableOnce[Double]) = {
    val res = learnNumber[Double]()(oscar.stochastic.toOperationabled, d)
    for (d <- list) res observe d
    res
  }

  def arrayGen = (Gen.containerOf[List, Double](Gen.choose(-10000000, 10000000).map(_.toDouble)) suchThat (_.map(a => a * a).sum < Double.PositiveInfinity))

  test("mean") {
    check {
      forAll(arrayGen) { arr: List[Double] =>
        val l = lNumber(arr)

        if (!arr.isEmpty) {
          l.nRea should be (arr.size)
          l.mean(l.nRea) should equal(arr.sum / arr.size)

          val mean = l.mean(l.nRea)
          l.variance(l.nRea) should be(arr.map(d => d * d).sum / arr.size - mean * mean plusOrMinus 0.00000001)
        }
        true
      }
    }
  }

  test("equals") {
    check {
      forAll(arrayGen) { arr: List[Double] =>
        lNumber(arr) should equal(lNumber(scala.util.Random.shuffle(arr)))
        true
      }
    }
  }

  test("aggregate") {
    check {
      forAll(arrayGen, arrayGen) { (arr1: List[Double], arr2: List[Double]) =>
        val l1 = lNumber(arr1)
        val l2 = lNumber(arr2)

        l1 aggregate l2

        for (d <- arr1) l2 observe d

        l1 should equal(l2)
        true

      }
    }
  }

}