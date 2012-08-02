/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.examples.linprog

import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._

/* 
 * The knapsack problem is a well-known problem in combinatorial optimization: 
 * Given a set of items, each with a weight and an utility, determine the count of each item 
 * to include in a collection so that the total weight is less than or equal to a given limit
 * and the total utility is as large as possible.
 * @author gme
 */
object KnapSack {
  
  def main(args: Array[String]) {
    
    case class O(val weight: Int, val utility: Int, val x: MIPVar)   
    val weights = Array(100,50,45,20,10,5)
    val utility = Array( 40,35,18, 4,10,2)
    
    val mip = MIPSolver(LPSolverLib.lp_solve)
    val objects = Array.tabulate(weights.size)(i => O(weights(i),utility(i),MIPVar(mip, "x"+i, 0 to 1)))
    
    val capacity = 100
    
    
    // maximize total utility
    mip.maximize(sum(objects)(o => o.x * o.utility)) subjectTo {
      
      // given the limited capacity of the pack
      mip.add(sum(objects)(o => o.x * o.weight) <= capacity)
      
    }
        
    val selected = objects.filter(o => o.x.getValue == 1)  
    var totalWeight = selected.map(o => o.weight).sum  
        
    println("Status: " + mip.status)
    println("Total Utility: " + mip.getObjectiveValue())
    println("Total Weight: " + totalWeight)
        
        
    mip.release()
  }

}
