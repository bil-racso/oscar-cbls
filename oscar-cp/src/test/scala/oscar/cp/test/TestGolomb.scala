package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._

import oscar.cp._
import collection.immutable.SortedSet

class TestGolomb extends FunSuite with ShouldMatchers {

  test("Golomb Ruler") {
    // return the best ruler with n ticks
    def test(n: Int) = {

      val cp = CPSolver()

      val marks = Array.fill(n)(CPIntVar(0 to n * n)(cp))

      val obj = marks(n - 1)
      var best = Int.MaxValue
      cp.silent = true
      cp.minimize(obj) subjectTo {
        // we break symmetries to put the marks increasing
        cp.add(marks(0) == 0)
        for (i <- 0 until n - 1) {
          cp.add(marks(i) < marks(i + 1))
        }

        cp.add(allDifferent(for (i <- 0 until n; j <- i + 1 until n) yield marks(j) - marks(i)), Strong);

        // break the symmetries between differences
        cp.add(marks(1) - marks(0) < marks(n - 1) - marks(n - 2));

      } search {
        binaryFirstFail(marks)
      } onSolution {
        best = obj.value
      } start ()
      best
    }

    test(6) should be(17)
    test(7) should be(25)

  }

}
