package oscar.linprog.test

import java.nio.file.Paths

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.algebra._
import oscar.linprog.enums.{LP, Optimal, SolutionFound}
import oscar.linprog.interface.MPSolverLib
import oscar.linprog.modeling._

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class PiecewiseLinearObjectiveTester extends OscarLinprogTester {

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |x|") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Abs(x, -100, 100), 200)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |-x|") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Abs(-x, -100, 100), 200)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize sign(x)") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Sign(x, -100, 100), 200)

    solver.solve should equal(SolutionFound)

    x.value.get should be < 0.0

    solver.objectiveValue.toOption should equalWithTolerance(Some(-1))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Maximize sign(x)") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    maximize(Sign(x, -100, 100), 200)

    solver.solve should equal(SolutionFound)

    x.value.get should be > 0.0

    solver.objectiveValue.toOption should equalWithTolerance(Some(1))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Update piecewise linear objective") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Abs(x, -100, 100), 200)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))

    val f = new PiecewiseLinearExpression(
      pieces = Seq(
        new LinearPiece( 2*x, x, Interval(-10, true, -5, false)),
        new LinearPiece(   x, x, Interval( -5, true,  2, false)),
        new LinearPiece(  -x, x, Interval(  2, true,  5, false)),
        new LinearPiece(-2*x, x, Interval(  5, true, 10, false))
      )
    )

    maximize(f, bigM = 200)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(2))

    solver.objectiveValue.toOption should equalWithTolerance(Some(2))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Replace linear objective by piecewise linear objective") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(2*x)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(-100.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(-200.0))
    solver.solutionQuality should equal(Success(Optimal))
    solver.objective should be ('defined)
    solver.piecewiseLinearObjective should not be ('defined)

    minimize(Abs(x, -100, 100), bigM = 200)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
    solver.objective should not be ('defined)
    solver.piecewiseLinearObjective should be ('defined)
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Replace piecewise linear objective by linear objective") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Abs(x, -100, 100), bigM = 200)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(0.0))
    solver.solutionQuality should equal(Success(Optimal))
    solver.objective should not be ('defined)
    solver.piecewiseLinearObjective should be ('defined)

    minimize(2*x)

    solver.solve should equal(SolutionFound)

    x.value should equalWithTolerance(Some(-100.0))

    solver.objectiveValue.toOption should equalWithTolerance(Some(-200.0))
    solver.solutionQuality should equal(Success(Optimal))
    solver.objective should be ('defined)
    solver.piecewiseLinearObjective should not be ('defined)
  }
}
