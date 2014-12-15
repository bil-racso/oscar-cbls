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
import oscar.algebra._

/**
 * One dimensional bin packing problem.
 * The bin packing problem consists in placing objects
 * of different sizes into a finite number of bins of
 * fixed capacity in a way that minimizes the number of bins used.
 * @author gme
 */
object BinPacking1D extends MIPModelGLPK with App {

  // maximal number of available bins
  val nBins = 50

  // size of each bin
  val binSize = 100

  val objectSizes = Array(94, 34, 97, 57, 50)
  val nObjects = objectSizes.length

  // binary variables: does object o goes to bin b ?
  val objectAssignations = Array.tabulate(nObjects, nBins)((o, b) => MIPIntVar("o" + o + "b" + b, 0 to 1))

  // binary variables: is bin b selected ?
  val binSelection = Array.tabulate(nBins)(b => MIPIntVar("b" + b, 0 to 1))

  // limited bin capacities
  for (j <- 0 until nBins)
    add(sum(0 until nObjects)(i => objectAssignations(i)(j) * objectSizes(i)) <= binSize * binSelection(j))

  // objects are assigned to only one bin
  for (i <- 0 until nObjects)
    add(sum(0 until nBins)(j => objectAssignations(i)(j)) == 1)

  minimize(sum(0 until nBins)(j => binSelection(j))) start ()
  println("Status: " + status)
  println("Objective: " + objectiveValue)

  println("Solution found:")
  for (j <- 0 until nBins)
    if (binSelection(j).value.get == 1) {
      println("Bin #" + j)
      for (i <- 0 until nObjects)
        if (objectAssignations(i)(j).value.get == 1)
          print(objectSizes(i) + " ")
      println
    }

}
