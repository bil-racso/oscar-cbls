/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package test.scala.scampi.linprog


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scampi.linprog.modeling._

/**
 * LPTesting
 */
class LPTest extends FunSuite with ShouldMatchers with LPModel with MIPModel {

    val solvers = List(LPSolverLib.lp_solve, LPSolverLib.glpk)
 
    test("lp test 1") {
    	for (lib <- solvers) {
    		val lp = new LPSolver(lib)
    		val x = new LPVar(lp,"x",100,200)
    		val y = new LPVar(lp,"y",80,170)

    		lp.maximize(-2*x+5*y) subjectTo {
    			lp.add(y >= -x + 200)
    		}

    		x.value should equal (Some(100))
    		y.value should equal (Some(170))
    		lp.getObjectiveValue() should equal(650)
    		lp.getStatus() should equal (LPStatus.OPTIMAL)
    		lp.release()
    	}
    }
	
	test("lp test 2") {
		for (lib <- solvers) {
			val lp = new LPSolver(lib)
			val x = new LPVar(lp,"x",100,200)
			val y = new LPVar(lp,"y",80,170)

			lp.minimize(-2*x+5*y) subjectTo {
				lp.add(y >= -x + 200)
			}

			x.value should equal (Some(200))
			y.value should equal (Some(80))
			lp.getObjectiveValue() should equal(0)
			lp.getStatus() should equal (LPStatus.OPTIMAL)
			lp.release()
		}
    } 
	
	test("lp test 3") {
		for (lib <- solvers) {
			val lp = new LPSolver(lib)
			val x = new LPVar(lp,"x")
			val y = new LPVar(lp,"y",80,170)

			lp.minimize(-2*x+5*y) subjectTo {
				lp.add(y >= -x + 200)
			}
			// the solution is infeasible but some solver consider it dual infeasible
			lp.getStatus() should (equal (LPStatus.UNBOUNDED) or equal (LPStatus.INFEASIBLE))
			lp.release()
		}
    }	
	
	test("lp test 4") {
		for (lib <- solvers) {
			val lp = new LPSolver(lib)
			val x = new LPVar(lp,"x",100,200)
			val y = new LPVar(lp,"y",80,170)
			var z: LPVar = null
			lp.minimize(-2*x+5*y) subjectTo {
			  z = new LPVar(lp,"z",80,170)
			  lp.add(z >= 170)
			  lp.add(y >= -x + 200)
			}

			x.value should equal (Some(200))
			y.value should equal (Some(80))
			z.value should equal (Some(170))
			lp.getObjectiveValue() should equal(0)
			lp.getStatus() should equal (LPStatus.OPTIMAL)
			lp.release()
		}
    } 
	
	test("lp test 5") {
		for (lib <- solvers) {
			val lp = new LPSolver(lib)
			val x = new LPVar(lp,"x",100,200)
			val y = new LPVar(lp,"y",80,170)
			lp.minimize(-2*x+5*y) subjectTo {
			  lp.add(y >= -x + 200)
			}

			x.value should equal (Some(200))
			y.value should equal (Some(80))
			lp.getObjectiveValue() should equal(0)
			lp.getStatus() should equal (LPStatus.OPTIMAL)
			lp.release()
		}
    } 
	
	test("lp test 6") {
		for (lib <- solvers) {
			val lp = new LPSolver(lib)
			val x = new LPVar(lp,"x",0,10)
			val y = new LPVar(lp,"y",0,10)
			lp.maximize(x+y) subjectTo {
			  lp.add(x+y >= 5)
			}

			x.value should equal (Some(10))
			y.value should equal (Some(10))
			lp.getObjectiveValue() should equal(20)
			lp.getStatus() should equal (LPStatus.OPTIMAL)
			
			x.setBounds(0,11)
			y.setBounds(0,11)
			
		    x.value should equal (Some(11))
			y.value should equal (Some(11))
			lp.getObjectiveValue() should equal(22)
			lp.getStatus() should equal (LPStatus.OPTIMAL)
			
			lp.release()
		}
    }	
	

	


}

