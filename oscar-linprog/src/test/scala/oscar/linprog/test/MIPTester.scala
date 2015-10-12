package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner
import oscar.linprog.enums.{Optimal, SolutionFound}
import oscar.linprog.interface.MIPSolverInterface
import oscar.linprog.modeling._

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class MIPTester extends FunSuite with Matchers {

  test("Maximize objective under constraints with integer variables") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val x = FloatVar("x", 0, 100)
      val y = IntVar("y", 0 to 100)

      maximize(8 * x + 12 * y)
      add(10 * x + 20 * y <= 140)
      add( 6 * x +  8 * y <=  72)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)
      solver.solutionQuality should equal(Success(Optimal))

      x.value.get should equal(8.0 +- 1e-6)
      y.value should equal(Some(3))

      solver.release()
    }
  }
}
