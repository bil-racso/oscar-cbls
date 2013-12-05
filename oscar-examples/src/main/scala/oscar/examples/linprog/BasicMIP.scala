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
package oscar.examples.linprog


import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._


object BasicMIP {
	
  def main(args: Array[String]): Unit = {  
	  
	  val mip = MIPSolver(LPSolverLib.lp_solve)
 
	  val x0 = MIPVar(mip,"x0",0.0,40.0)
	  val x1 = MIPVar(mip,"x1",0 to 1000) 
	  val x2 = MIPVar(mip,"x2",0 until 18) 
	  val x3 = MIPVar(mip,"x3",2.0,3.0)	 
	   
	   
	  mip.maximize(x0+2*x1+3*x2+x3) subjectTo {
	 	  mip.add(-1*x0 + x1 + x2 + 10*x3 <= 20)
	 	  mip.add(x0 - 3.0*x1 + x2 <= 30)
	 	  mip.add(x1 - 3.5*x3 == 0 )
	  }
      val x0v = x0.getValue
      val x1v = x1.getValue
      val x2v = x2.getValue
      val x3v = x3.getValue
	  println("objective: "+mip.getObjectiveValue())
	  println("x0= "+x0.getValue+" "+x1.value)
	  println("x1= "+x1.getValue)
	  println("x2= "+x2.getValue)
	  println("x3= "+x3.getValue)
	  
	  mip.release()
  }
}
