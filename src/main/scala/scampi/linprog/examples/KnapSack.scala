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

/* 
 * The knapsack problem is a well-known problem in combinatorial optimization: 
 * Given a set of items, each with a weight and an utility, determine the count of each item 
 * to include in a collection so that the total weight is less than or equal to a given limit
 * and the total utility is as large as possible.
 * @author gme
 */
object KnapSack extends MIPModel {
  
  def main(args: Array[String]) {
    
    case class O(val weight: Int, val utility: Int, val x: MIPVar)   
    val weights = Array(100,50,45,20,10,5)
    val utility = Array( 40,35,18, 4,10,2)
    
    val mip = MIPSolver(LPSolverLib.glpk)
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
