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


/** 
 * Assign workers to shifts while satisfying requirements for that day. 
 * Each worker may or may not be available on a particular day.
 * The objective is to minimize the total pay costs 
 * (Example Inspired from Gurobi Website.)
 * @author Pierre Schaus pschaus@gmail.com 
 */

object Workforce extends MIPModel{
	
  def main(args: Array[String]): Unit = {  
	  

	  val mip = MIPSolver(LPSolverLib.glpk)
	   
      val Shifts = 0 to 13
      val Workers = 0 to 6
      val shiftRequirements = Array( 3, 2, 4, 1, 5, 2, 4, 2, 2, 3, 4, 5, 3, 5)
      val pay = Array (10, 12, 10, 8, 8, 9, 11 ) // salary of each persons when they are scheduled
      val maxNbShift = 7 // maximum number of shift a worker can be assigned to in the schedule
      
      val availability =
      Array (Array ( 0, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1 ),
              Array( 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0 ),
              Array( 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1 ),
              Array( 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1 ),
              Array( 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1 ),
              Array( 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1 ),
              Array( 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ) )
              
	 val assigned = Array.tabulate(Workers.size,Shifts.size)((_, _) => MIPVar(mip,"",0 to 1))
	 
	 mip.minimize(sum(Workers,Shifts)((w,s) =>  assigned(w)(s)*pay(w))) subjectTo {
		 for (s <-0 until Shifts.size) {
		   mip.add(sum(Workers)(w => assigned(w)(s) * availability(w)(s)) == shiftRequirements(s))
		 }	 
		 for (w <- Workers) {
		   mip.add(sum(Shifts)(s => assigned(w)(s)) <= maxNbShift)
		 }

	 }
  
	 println("objective: "+mip.getObjectiveValue())
	 for(s <-0 until Shifts.size){
	    println("Day "+s+" workers: " + Workers.filter(assigned(_)(s).getValue == 1))
	 }
	 mip.release()

  }
  
}
