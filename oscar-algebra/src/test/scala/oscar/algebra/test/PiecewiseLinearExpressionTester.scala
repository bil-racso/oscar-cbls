package oscar.algebra.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.algebra.{LinearPiece, Interval, PiecewiseLinearExpression}

@RunWith(classOf[JUnitRunner])
class PiecewiseLinearExpressionTester extends OscarAlgebraTester {

  val x = TestVar("x", 0.0)

  val pcwle1 =
    new PiecewiseLinearExpression(
     pieces = Seq(
       new LinearPiece(2.0, x, Interval(-Double.MaxValue, lbInclusive = false,  0.0, ubInclusive = false)),
       new LinearPiece(x - 4.0, x, Interval(0.0, lbInclusive = true,  4.0, ubInclusive = false)),
       new LinearPiece(-x, x, Interval(4.0, lbInclusive = true,  Double.MaxValue, ubInclusive = false))
     )
    )

  testValue(pcwle1,  2.0, x -> -4.0)
  testValue(pcwle1, -4.0, x -> 0.0)
  testValue(pcwle1, -2.0, x -> 2.0)
  testValue(pcwle1, -4.0, x -> 4.0)
  testValue(pcwle1, -8.0, x -> 8.0)

  val gx = 2 * x + 1

  val pcwle2 =
    new PiecewiseLinearExpression(
      pieces = Seq(
        new LinearPiece(gx, gx, Interval(-Double.MaxValue, lbInclusive = false,  0.0, ubInclusive = false)),
        new LinearPiece(0, gx, Interval(0.0, lbInclusive = true,  10.0, ubInclusive = false)),
        new LinearPiece(0.5 * gx - 2, gx, Interval(10.0, lbInclusive = true,  20.0, ubInclusive = false)),
        new LinearPiece(-2 * gx, gx, Interval(20.0, lbInclusive = true,  Double.MaxValue, ubInclusive = false))
      )
    )

  testValue(pcwle2, gx.eval(x => -4.0)          , x -> -4.0)
  testValue(pcwle2, 0.0                         , x -> 0.0)
  testValue(pcwle2, 0.0                         , x -> 2.0)
  testValue(pcwle2, 0.5 * gx.eval(x => 9.0) - 2 , x -> 9.0)
  testValue(pcwle2, -2 * gx.eval(x => 9.5)      , x -> 9.5)
  testValue(pcwle2, -2 * gx.eval(x => 10.0)     , x -> 10.0)
  testValue(pcwle2, -2 * gx.eval(x => 15.0)     , x -> 15.0)

  testValue(2 * pcwle1,  2.0 * 2, x -> -4.0)
  testValue(2 * pcwle1, -4.0 * 2, x -> 0.0)
  testValue(2 * pcwle1, -2.0 * 2, x -> 2.0)
  testValue(2 * pcwle1, -4.0 * 2, x -> 4.0)
  testValue(2 * pcwle1, -8.0 * 2, x -> 8.0)

  testValue(pcwle1 + 1,  2.0 + 1, x -> -4.0)
  testValue(pcwle1 + 1, -4.0 + 1, x -> 0.0)
  testValue(pcwle1 + 1, -2.0 + 1, x -> 2.0)
  testValue(pcwle1 + 1, -4.0 + 1, x -> 4.0)
  testValue(pcwle1 + 1, -8.0 + 1, x -> 8.0)

  testValue(pcwle1 - 1,  2.0 - 1, x -> -4.0)
  testValue(pcwle1 - 1, -4.0 - 1, x -> 0.0)
  testValue(pcwle1 - 1, -2.0 - 1, x -> 2.0)
  testValue(pcwle1 - 1, -4.0 - 1, x -> 4.0)
  testValue(pcwle1 - 1, -8.0 - 1, x -> 8.0)

  testValue(pcwle1 + x,  2.0 + -4, x -> -4.0)
  testValue(pcwle1 + x, -4.0 + 0, x -> 0.0)
  testValue(pcwle1 + x, -2.0 + 2, x -> 2.0)
  testValue(pcwle1 + x, -4.0 + 4, x -> 4.0)
  testValue(pcwle1 + x, -8.0 + 8, x -> 8.0)

  testValue(pcwle1 + pcwle2,  2.0 + gx.eval(x => -4.0), x -> -4.0)
  testValue(pcwle1 + pcwle2, -4.0 + 0.0, x -> 0.0)
  testValue(pcwle1 + pcwle2, -9.0 + 0.5 * gx.eval(x => 9.0) - 2 , x -> 9.0)
  testValue(pcwle1 + pcwle2, -9.5 + -2 * gx.eval(x => 9.5)      , x -> 9.5)
}
