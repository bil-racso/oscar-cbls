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

      val x = IntVar("x", 0 to 100)
      val y = FloatVar("y", 0, 100)

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

  test("Maximize objective under constraints with binary variables") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val x = BinaryVar("x")
      val y = FloatVar("y", 0, 100)

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
