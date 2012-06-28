/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.linprog.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._

/**
 * MIP Testing
 */
class MIPTest extends FunSuite with ShouldMatchers with LPModel with MIPModel {

    val solvers = List(LPSolverLib.lp_solve, LPSolverLib.glpk, LPSolverLib.cplex)
 
    test("mip test 1") {
    	for (lib <- solvers) {
 
    		val mip = new MIPSolver()
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

