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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Yoann Guyot
  ******************************************************************************/

package oscar.cbls.invariants.tests

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import oscar.cbls.constraints.lib.global._

class ConstraintTests extends FunSuite with Checkers{

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvariantTestBench
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }

  test("AtLeast") {
    val bench = new InvariantTestBench
    new AtLeast(bench.genIntVars(10), bench.genBoundedValues(10, 0 to 30, 0 to 10)).toIntVar
    bench.run
  }

  test("AtMost") {
    val bench = new InvariantTestBench
    new AtMost(bench.genIntVars(10), InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)).toIntVar
    bench.run
  }

  test("MultiKnapsack") {
    val bench = new InvariantTestBench
    //E elements, B bins
    val e:Int = 10
    val b:Int = 5
    new MultiKnapsack(
      bench.genIntVarsArray(e,0 to (b-1)),
      bench.genIntVarsArray(e,0 to 100),
      bench.genIntVarsArray(b,0 to 200)).toIntVar
    bench.run
  }

  test("Sequence") {
    val bench = new InvariantTestBench
    new Sequence(
      bench.genIntVarsArray(20,0 to 10),
      13,
      5,
      (x: Int) => x > 1).toIntVar
    bench.run
  }

}