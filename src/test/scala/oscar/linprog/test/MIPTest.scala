/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.linprog.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._
import oscar.linprog._

/**
 * MIP Testing
 */
class MIPTest extends FunSuite with ShouldMatchers with LPModel with MIPModel {

 
    test("mip test 1") {
    	for (lib <- solvers) {
 
    		val mip = new MIPSolver(lib)
    		val x = new MIPVar(mip,"x",0 , 100)
    		val y = new MIPVar(mip,"y",0 to 100)

    		mip.maximize(8 * x + 12 * y) subjectTo {
    			mip.add(10 * x + 20 * y <= 140)
    			mip.add(6 * x + 8 * y <= 72)
    		}
    		
    		mip.getStatus() should equal (LPStatus.OPTIMAL)
    		x.getValue should be (8.0 plusOrMinus 0.00001)
    		y.value should equal (Some(3))
			mip.release()


    	}
    }

}

