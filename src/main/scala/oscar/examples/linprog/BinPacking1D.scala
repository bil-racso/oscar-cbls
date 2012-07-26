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

/**
 * One dimensional bin packing problem.
 * The bin packing problem consists in placing objects 
 * of different sizes into a finite number of bins of 
 * fixed capacity in a way that minimizes the number of bins used.
 * @author gme
 */
object BinPacking1D {

  def main(args: Array[String]) {
    
    val mip = MIPSolver(LPSolverLib.glpk)
    
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
