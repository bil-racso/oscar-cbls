package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import oscar.algebra.Abs
import oscar.linprog.enums.{Optimal, SolutionFound}
import oscar.linprog.interface.MPSolverLib
import oscar.linprog.modeling._

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class PiecewiseLinearObjectiveTester extends OscarLinprogTester {

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |x|") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Abs(x, -100, 100), bigM = 100)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue should equal(Success(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }

  testForAllSolvers(MPSolverLib.mipSolvers, "Minimize |-x|") { implicit solver =>
    val x = MPFloatVar("x", -100, 100)

    minimize(Abs(-x, -100, 100), bigM = 100)

    val endStatus = solver.solve

    endStatus should equal(SolutionFound)

    x.value should equalWithTolerance(Some(0.0))

    solver.objectiveValue should equal(Success(0.0))
    solver.solutionQuality should equal(Success(Optimal))
  }
}
