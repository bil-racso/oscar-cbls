/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples


import scampi.linprog.modeling._


object BasicLP extends LPModel{
	
  def main(args: Array[String]): Unit = {  
      
	  val lp = LPSolver()
	   
	  val x0 = LPVar(lp,"x0",0,40)
	  val x1 = LPVar(lp,"x1",0, 1000) 
	  val x2 = LPVar(lp,"x2",0 ,17) 
	  val x3 = LPVar(lp,"x3",2,3)	 
	   
	   
	  lp.maximize(x0+2*x1+3*x2+x3) subjectTo {
	 	  lp.add(-1*x0 + x1 + x2 + 10*x3 <= 20)
	 	  lp.add(x0 - 3.0*x1 + x2 <= 30)
	 	  lp.add(x1 - 3.5*x3 == 0 )
	  }
  
	  println("objective"+lp.getObjectiveValue())
	  
	  lp.release()
	  
  }
  
  


}
