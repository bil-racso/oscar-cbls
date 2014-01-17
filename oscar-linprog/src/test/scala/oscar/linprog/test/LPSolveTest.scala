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

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import oscar.linprog.modeling._
import org.scalatest.BeforeAndAfter

import java.io.{ File, PrintWriter, FileWriter }

/**
 * Testing OscaR's handling of lp_solve specific settings 
 * @author Alastair Andrew
 */
class LPSolveTest extends FunSuite with ShouldMatchers with BeforeAndAfter {
	
    val tmpConfigFile = File.createTempFile("lp_solve", ".ini")
    val writer = new PrintWriter(new FileWriter(tmpConfigFile))
  
    before {
    	writer.println("[Default]") // Must match readParams argument in LPSolve.scala
    	writer.println("verbose=CRITICAL") // lp_solve is silent (other than errors).
    }
    
    after {
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
      release()
      
      mip.status should be(LPStatus.SUBOPTIMAL)
	  
	}
}