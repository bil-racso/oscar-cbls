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
import oscar.cp.constraints.{AllDiffAC, AllDiffFWC, CountingBasedAllDifferent}


class TestCountingBasedAllDifferent extends FunSuite with ShouldMatchers  {


  test("CountingBasedAllDifferent : small") {
    implicit val cp = CPSolver()
    val x = Array.fill(2)(CPIntVar(0 to 1))
    cp.post(new CountingBasedAllDifferent(x))
    cp.search(binaryStatic(x))
    val stat = cp.start()
    assert(stat.nSols == 2)
  }



  
  val rand = new scala.util.Random(0)
  
  def randomDom(size: Int) = {
    //Array.fill(size)(rand.nextInt(size)).toSet

    val offset = rand.nextInt(size) // to get some negative values

    val min = rand.nextInt(size) - offset
    val max = (min + rand.nextInt(3)) max(size-1) - offset
    (min to max).toSet
  }

  test("CountingBasedAllDifferent : random") {
    for (i <- 0 until 200) {
      val cp = CPSolver()
      val n = 6
      val x = Array.tabulate(n)(i => CPIntVar(randomDom(n+1))(cp))

      cp.pushState()

      cp.search(binaryStatic(x))

      val stat1 = cp.startSubjectTo() {
        cp.add(new AllDiffFWC(x))
      }
      val stat2 = cp.startSubjectTo() {
        cp.post(new CountingBasedAllDifferent(x))
      }

      val stat3 = cp.startSubjectTo() {
        cp.post(new AllDiffAC(x))
      }

      assert(stat1.nSols == stat2.nSols)
      assert(stat1.nFails >= stat2.nFails)
      assert(stat2.nFails >= stat3.nFails)
      
    }

    
    
    
  }

  

}
