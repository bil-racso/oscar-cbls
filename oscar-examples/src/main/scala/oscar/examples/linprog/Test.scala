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

object Test{
	
  def main(args: Array[String]): Unit = {  
      
    
	  val lp = LPSolver()
	   	  
	  val x1 = LPVar(lp,"x1")
	  val x2 = LPVar(lp,"x2")
	  val x3 = LPVar(lp,"x3")
	  val x4 = LPVar(lp,"x4")
	  val x5 = LPVar(lp,"x5")
	  

	  val expr0 = -2 * x3 -2 * x1 - x2 + x3 + x1 -1 + 1* x3 // equal -x1 -x2 -1
	  val expr1 = -x1-x2-1
	  
	  println(expr0.coef + " " + expr1.coef)


  
	  
  }
  
  


}
