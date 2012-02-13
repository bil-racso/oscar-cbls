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
 * One dimensional bin packing problem.
 * The bin packing problem consists in placing objects 
 * of different sizes into a finite number of bins of 
 * fixed capacity in a way that minimizes the number of bins used.
 * @author gme
 */
object BinPacking1D extends MIPModel {

  def main(args: Array[String]) {
    
    val mip = new MIPSolver(LPSolverLib.glpk)
    
    // maximal number of available bins
    val nBins = 50
    
    // size of each bin
    val binSize = 100
    
    val objectSizes = Array(94,34,97,57,50)
    val nObjects = objectSizes.length
    
    // binary variables: does object o goes to bin b ?
    val objectAssignations = Array.tabulate(nObjects, nBins) ((o, b) => MIPVar(mip, "o" + o + "b" + b, 0 to 1))  
    
    // binary variables: is bin b selected ?
    val binSelection = Array.tabulate(nBins) (b => MIPVar(mip, "b" + b, 0 to 1))
    
    mip.minimize(sum(0 until nBins) (j => binSelection(j))) subjectTo {
      
      // limited bin capacities
      for (j <- 0 until nBins)
        mip.add(sum(0 until nObjects) (i => objectAssignations(i)(j)*objectSizes(i)) <= binSize*binSelection(j))
            
      // objects are assigned to only one bin
      for (i <- 0 until nObjects)
        mip.add(sum(0 until nBins) (j => objectAssignations(i)(j)) == 1)
        
    } 
    
    println("Status: " + mip.status)
    println("Objective: " + mip.getObjectiveValue())
    
    println("Solution found:")
    for (j <- 0 until nBins)
      if (binSelection(j).getValue == 1) {
        println("Bin #" + j)
        for (i <- 0 until nObjects)
          if (objectAssignations(i)(j).getValue == 1)
            print(objectSizes(i) + " ")
        println
  	  } 
  }
}
