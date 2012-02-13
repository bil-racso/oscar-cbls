/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples


import scampi.linprog.modeling._


object Test extends LPModel{
	
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
