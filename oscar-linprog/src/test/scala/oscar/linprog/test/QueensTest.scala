/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.linprog.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._

/**
 * MIP Testing
 */
class QueensTest extends FunSuite with ShouldMatchers {

  test("Queens") {
    for (lib <- solvers) {

      val n = 8
      val Lines = 0 until n
      val Columns = 0 until n
      implicit val mip = MIPSolver(lib)
      val x = Array.tabulate(n, n)((l, c) => MIPVar("x" + (l, c), 0 to 1))

      maximize(sum(Lines, Columns) { (l, c) => x(l)(c) })

      /* at most one queen can be placed in each row */
      for (l <- Lines)
        add(sum(Columns)(c => x(l)(c)) <= 1)
      /* at most one queen can be placed in each column */
      for (c <- Columns)
        add(sum(Lines)(l => x(l)(c)) <= 1)

      /* at most one queen can be placed in each "/"-diagonal  upper half*/
      for (i <- 1 until n)
        add(sum(0 to i)((j) => x(i - j)(j)) <= 1)

      /* at most one queen can be placed in each "/"-diagonal  lower half*/
      for (i <- 1 until n)
        add(sum(i until n)((j) => x(j)(n - 1 - j + i)) <= 1)

      /* at most one queen can be placed in each "/"-diagonal  upper half*/
      for (i <- 0 until n)
        add(sum(0 until n - i)((j) => x(j)(j + i)) <= 1)

      /* at most one queen can be placed in each "/"-diagonal  lower half*/
      for (i <- 1 until n)
        add(sum(0 until n - i)((j) => x(j + i)(j)) <= 1)

      start()

      status should equal(LPStatus.OPTIMAL)
      objectiveValue.get should be(8.0 plusOrMinus 0.00001)

      println("objective: " + objectiveValue)

      for (i <- 0 until n) {
        for (j <- 0 until n)
          if (x(i)(j).value.get == 1) print("Q") else print(".")
        println()
      }
      release()

      checkConstraints() should be(true)

    }
  }

}

