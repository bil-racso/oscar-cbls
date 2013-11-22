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

object BasicLP {
	
  def main(args: Array[String]): Unit = {  
      
	  val lp = LPSolver()
	   
	  val x0 = LPVar(lp,"x0",0,40)
	  val x1 = LPVar(lp,"x1",0, 1000) 
	  val x2 = LPVar(lp,"x2",0 ,17) 
	  val x3 = LPVar(lp,"x3",2,3)	 
	   
	  var cons = Array[LPConstraint]() 
	  lp.maximize(x0+2*x1+3*x2+x3) subjectTo {
	 	  cons = cons :+ lp.add(-1*x0 + x1 + x2 + 10*x3 <= 20, "cons1")
	 	  cons = cons :+ lp.add(x0 - 3.0*x1 + x2 <= 30, "cons2")
	 	  cons = cons :+ lp.add(x1 - 3.5*x3 == 0)
	  }
	  
	  println("x0:"+x0.value)
	  println("x1:"+x1.value)
	  println("x2:"+x2.value)
	  println("x3:"+x3.value)
	  
	  
	  for (c <- cons) {
	    println("-------------")
	    println(c.name)
	    println(c.slack())
	    println(c.isTight())
	    println(c.check())
	  }
  
	  println("objective"+lp.getObjectiveValue())
	  
	  lp.release()
	  
  }
  
  


}
