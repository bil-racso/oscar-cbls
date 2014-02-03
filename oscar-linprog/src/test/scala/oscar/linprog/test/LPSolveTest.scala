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

import java.io.{File, FileWriter, PrintWriter}

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers

import oscar.algebra.{double2const, int2const, sum}
import oscar.linprog.modeling.{LPSolve, LPSolverLib, LPStatus, MIPFloatVar, MIPIntVar, MIPSolver, add, maximize, release, start}

import lpsolve._

/**
 * Testing OscaR's handling of lp_solve specific settings 
 * @author Alastair Andrew
 */
class LPSolveTest extends FunSuite with ShouldMatchers with BeforeAndAfter {
	
    var tmpConfigFile: File = _
    var writer: PrintWriter = _
  
    before {
        tmpConfigFile = File.createTempFile("lp_solve", ".ini")
        writer = new PrintWriter(new FileWriter(tmpConfigFile))
    	writer.println("[Default]") // Must match readParams argument in LPSolve.scala
    	writer.println("verbose=CRITICAL") // lp_solve is silent (other than errors).
    }
    
    after {
        writer.close()
    	tmpConfigFile.delete()
    }
    
    test("Test lp_solve correctly returns solution when status is LPStatus.SUBOPTIMAL") {
	  
        implicit val mip = MIPSolver(LPSolverLib.lp_solve)
	
	val lpSolve = mip.solver match {
	    case lp: LPSolve => lp
	    case _ => fail("Test case requires that solver is actually lp_solve")
	}
	  
	writer.println("break_at_first=1")
	writer.close() // Writer must be closed to allow lp_solve to read it
	  
	lpSolve.configFile = tmpConfigFile
	
        val x0 = MIPFloatVar("x0", 0, 40)
        val x1 = MIPIntVar("x1", 0 to 1000) 
        val x2 = MIPIntVar("x2", 0 until 18)
        val x3 = MIPFloatVar("x3", 2, 3)  

        maximize(x0 + 2*x1 + 3*x2 + x3) 
        add(-1*x0 + x1 + x2 + 10*x3 <= 20)
        add(x0 - 3.0*x1 + x2 <= 30)
        add(x1 - 3.5*x3 == 0)  
        start()
      
        mip.status should be(LPStatus.SUBOPTIMAL)
        
        lpSolve.lp.setBreakAtFirst(false)
        mip.solveModel()    
        	  
        mip.status should be(LPStatus.OPTIMAL)
        release()
	}
	
	/** Example taken from [[http://lpsolve.sourceforge.net/5.5/SOS.htm lp_solve's documentation SOS section]]
	 */
	test("Test lp_solve's SOS1 constraint handling") {
	    writer.close() // Not using additional config
		implicit val mip = MIPSolver(LPSolverLib.lp_solve)
		
		val lpSolve = mip.solver match {
			case lp: LPSolve => lp
			case _ => fail("Test case requires that solver is actually lp_solve")
		}
		
		lpSolve.configFile = tmpConfigFile
		
		val x = Array(MIPFloatVar("x1", 0, 40), 
					  MIPFloatVar("x2", 0, 1), 
					  MIPFloatVar("x3"), 
					  MIPFloatVar("x4"),
					  MIPFloatVar("x5", 0, 1))
		
		mip.minimize(-x(0) - x(1) - 3 * x(2) - 2 * x(3) - 2 * x(4))
		mip.add(-x(0) - x(1) + x(2) + x(3) <= 30, "c1")
		mip.add(x(0) + x(2) - 3 * x(3) <= 30, "c2")
		val sos1Cstr = x(0) + x(1) + x(2) + x(3) + x(4) <= 1
		mip.addSOS(sos1Cstr, "SOS")
		start()
		release()
		
		mip.status() should be(LPStatus.OPTIMAL)
		x(0).value should be(Some(0.0))
		x(1).value should be(Some(0.0))
		x(2).value should be(Some(30.0))
		x(3).value should be(Some(0.0))
		x(4).value should be(Some(0.0))
		mip.objectiveValue() should be(Some(-90))	
	}
	
	test("Test lp_solve's SOS1 constraint handling (as sum)") {
		writer.close() // Not using additional config
	    implicit val mip = MIPSolver(LPSolverLib.lp_solve)
		
		val lpSolve = mip.solver match {
			case lp: LPSolve => lp
			case _ => fail("Test case requires that solver is actually lp_solve")
		}
		
		lpSolve.configFile = tmpConfigFile
		
		val x = Array(MIPFloatVar("x1", 0, 40), 
					  MIPFloatVar("x2", 0, 1), 
					  MIPFloatVar("x3"), 
					  MIPFloatVar("x4"),
					  MIPFloatVar("x5", 0, 1))
		
		mip.minimize(-x(0) - x(1) - 3 * x(2) - 2 * x(3) - 2 * x(4))
		mip.add(-x(0) - x(1) + x(2) + x(3) <= 30, "c1")
		mip.add(x(0) + x(2) - 3 * x(3) <= 30, "c2") 
		mip.addSOS(sum(x) <= 1, "SOS1")
		start()
		release()
		
		mip.status() should be(LPStatus.OPTIMAL)
		x(0).value should be(Some(0.0))
		x(1).value should be(Some(0.0))
		x(2).value should be(Some(30.0))
		x(3).value should be(Some(0.0))
		x(4).value should be(Some(0.0))
		mip.objectiveValue() should be(Some(-90))	
	}
	
	/** Took the example from CPLEX 12.2 Manual
	 */
	test("Test SOS1 weighting support - warehouse location example") {
	    writer.close() // Not using additional config
	    implicit val mip = MIPSolver(LPSolverLib.lp_solve)
		
		val lpSolve = mip.solver match {
			case lp: LPSolve => lp.configFile = tmpConfigFile
			case _ => fail("Test case requires that solver is actually lp_solve")
		}
		
		//lpSolve.configFile = tmpConfigFile
		
		val x = Array(MIPFloatVar("x1", 0, 40), 
					  MIPFloatVar("x2", 0, 1), 
					  MIPFloatVar("x3"), 
					  MIPFloatVar("x4"),
					  MIPFloatVar("x5", 0, 1))
		
		mip.minimize(-x(0) - x(1) - 3 * x(2) - 2 * x(3) - 2 * x(4))
		mip.add(-x(0) - x(1) + x(2) + x(3) <= 30, "c1")
		mip.add(x(0) + x(2) - 3 * x(3) <= 30, "c2") 
		mip.addSOS(sum(x) <= 1, "SOS1")
		mip.start(30)
		mip.release()
		
		mip.status() should be(LPStatus.OPTIMAL)
		x(0).value should be(Some(0.0))
		x(1).value should be(Some(0.0))
		x(2).value should be(Some(30.0))
		x(3).value should be(Some(0.0))
		x(4).value should be(Some(0.0))
		mip.objectiveValue() should be(Some(-90))
	}
}
