package oscar.algebra.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, FunSuite}
import oscar.algebra.Expression

class OscarAlgebraTester extends FunSuite with Matchers {

  def checkValue(expr: Expression, expected: Double, v: (TestVar, Double)*) = {
    v.foreach { x =>
      x._1.value = x._2
    }
    val vOpt = expr.value
    vOpt should be ('defined)
    vOpt.get should equal (expected +- 1e-6)

    val valuesMap = v.toSeq.toMap
    expr.eval { x => valuesMap(x.asInstanceOf[TestVar]) } should equal (expected +- 1e-6)
  }

  def testValue(expr: Expression, expected: Double, v: (TestVar, Double)*) =
    test(s"$expr for (${v.map{x => s"${x._1.name} = ${x._2}"}.mkString(", ")}) should equal $expected") {
      checkValue(expr, expected, v:_*)
    }
}
