package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner
import oscar.linprog.enums.{Optimal, SolutionFound}
import oscar.linprog.modeling._

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class MIPTester extends FunSuite with Matchers with OscarLinprogMatchers {

  test("Maximize objective under constraints with integer variables") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val x = MPIntVar("x", 0 to 100)
      val y = MPFloatVar("y", 0, 100)

      maximize(100 * x + 1 * y)
      add(3 * x + 1 * y <= 14.5)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)
      solver.solutionQuality should equal(Success(Optimal))

      x.value should equalWithTolerance(Some(4.0))
      y.value should equalWithTolerance(Some(14.5 - 3*4))

      solver.release()
    }
  }

  test("Change variable type from integer to continuous") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val x = MPIntVar("x", 0 to 100)
      val y = MPFloatVar("y", 0, 100)

      maximize(100 * x + 1 * y)
      add(3 * x + 1 * y <= 14.5)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      // Set x as a continuous var => performs the linear relaxation of the above problem
      x.setFloat()

      // Both variables should be continuous
      x.float should equal(true)
      y.float should equal(true)

      // Model has changed
      solver.solved should equal(false)
      solver.hasSolution should equal(false)

      // Reoptimize to get new solution
      val endStatus2 = solver.solve

      endStatus2 should equal(SolutionFound)
      solver.solutionQuality should equal(Success(Optimal))

      x.value should equalWithTolerance(Some(14.5/3.0))
      y.value should equalWithTolerance(Some(0.0))

      solver.release()
    }
  }

  test("Change variable type from continuous to integer") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val x = MPIntVar("x", 0, 100)
      val y = MPFloatVar("y", 0, 100)
      val z = MPFloatVar("z", 0, 100)

      maximize(1 * x + 2 * y + 3 * z)
      add(x + y <= 75.5)
      add(x + z <= 75.5)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      // Set y as an integer var
      y.setInteger()

      // Both variables should be integer
      x.integer should equal(true)
      y.integer should equal(true)
      z.integer should equal(false)

      // Model has changed
      solver.solved should equal(false)
      solver.hasSolution should equal(false)

      // Reoptimize to get new solution
      val endStatus2 = solver.solve

      endStatus2 should equal(SolutionFound)
      solver.solutionQuality should equal(Success(Optimal))

      x.value should equalWithTolerance(Some(0.0))
      y.value should equalWithTolerance(Some(75.0))
      z.value should equalWithTolerance(Some(75.5))

      solver.release()
    }
  }

  test("Maximize objective under constraints with binary variables") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val x = MPBinaryVar("x")
      val y = MPFloatVar("y", 0, 100)

      maximize(100 * x + 1 * y)
      add(3 * x + 1 * y <= 14.5)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)
      solver.solutionQuality should equal(Success(Optimal))

      x.value should equalWithTolerance(Some(1))
      y.value should equalWithTolerance(Some(14.5 - 3*1))

      solver.release()
    }
  }
}
