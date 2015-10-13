package oscar.linprog.test

import org.junit.runner.RunWith
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.junit.JUnitRunner
import oscar.linprog.enums._
import oscar.linprog.modeling._
import oscar.algebra._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Success

@RunWith(classOf[JUnitRunner])
class AbortTester extends FunSuite with Matchers with OscarLinprogMatchers {

  test("Call to abort AFTER solve") {
    for (_solver <- MPSolver.mipSolvers) {
      implicit val solver = _solver

      val n = 200

      val xs = Array.tabulate(n, n)((i, j) => MPBinaryVar(s"x[$i,$j]"))
      val ys = Array.tabulate(n)(i => MPBinaryVar(s"y$i"))

      val minSize = 10
      val maxSize = 100
      val sizes = Array.tabulate(n)(i => minSize + scala.util.Random.nextInt(maxSize - minSize))
      val binSize = n * maxSize / 2

      minimize(sum(ys))
      subjectTo {
        (for {
          i <- 0 until n
        } yield {
          s"allocation[$i]" -> (sum(0 until n)(j => xs(i)(j) * sizes(j)) <= ys(i) * binSize)
        }) ++ (
        for {
          j <- 0 until n
        } yield {
          s"unicity[$j]" -> (sum(0 until n)(i => xs(i)(j)) == Const(1.0))
        })
      }

      import scala.concurrent.ExecutionContext.Implicits.global
      val endStatusFuture = Future {
        solver.solve
      }

      // wait
      Thread.sleep(500)

      // abort
      solver.abort()

      //get the results
      val endStatus = Await.result(endStatusFuture, 1 minute)

      if(endStatus == Solution) {
        solver.solutionQuality should not equal (Success(Optimal))
      } else {
        endStatus should equal(NoSolution)
      }

      solver.release()
    }
  }

  test("Call to abort BEFORE solve") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver = _solver

      val x = MPFloatVar("x", 100, 150)
      val y = MPFloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(x + y <= 200)

      // Abort before solve should not prevent it
      solver.abort()

      val endStatus = solver.solve

      endStatus should equal(Solution)

      x.value should equalWithTolerance(Some(100))
      y.value should equalWithTolerance(Some(100))

      solver.objectiveValue should equal(Success(-2*100 + 5*100))
      solver.solutionQuality should equal(Success(Optimal))

      solver.release()
    }
  }
}
