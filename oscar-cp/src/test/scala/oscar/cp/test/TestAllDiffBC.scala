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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp._
import oscar.cp.constraints.AllDiffBC


class TestAllDiffBC extends FunSuite with ShouldMatchers  {
  
  
  test("test1") { 
	  val cp = CPSolver()
	  val x = Array(CPIntVar(3 to 4)(cp),CPIntVar(2 to 4)(cp),CPIntVar(3 to 4)(cp),CPIntVar(2 to 5)(cp),CPIntVar(1 to 6)(cp))
	  cp.add(new AllDiffBC(x))
  }
  
  val rand = new scala.util.Random(0)
  
  def randomDom(size: Int) = {
    //Array.fill(size)(rand.nextInt(size)).toSet
    val min = rand.nextInt(size)
    val max = (min + rand.nextInt(3)) max(size-1)
    (min to max).toSet
  }

  test("AllDiffBC") {
    for (i <- 0 until 200) {
      val cp = CPSolver()
      val n = 6
      val x = Array.tabulate(n)(i => CPIntVar(randomDom(n+1))(cp))

      cp.pushState()

      cp.search(binaryStatic(x))

      val stat1 = cp.startSubjectTo() {
        cp.add(allDifferent(x), Strong)
      }
      val stat2 = cp.startSubjectTo() {
        cp.add(new AllDiffBC(x))
      }
      stat1.nSols should be(stat2.nSols)
    }

    
    
    
  }

  

}
