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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/
package oscar.cbls.routing.neighborhood

import oscar.cbls.invariants.core.computation.IntVar

/**
 * Describes a particular neighbor.
 *
 * Info : a neighbor could be saw as a move operator.
 */
abstract class Neighbor{

  /**
   * Returns the objective value in case if we perform the move. (choose this neighbor)
   * @return the objective value if we choose this neighbor.
   */
  def getObjAfter:Int

  /**
   * It performs the move (which consist to choose this neighbor) on the VRP problem.
   */
  def comit()

  /**
   * Returns the node on which we restart the search procedure.
   * @return the node on which we restart the search procedure.
   */
  def startNodeForNextExploration:Int

  /**
   * Returns the variables and their values to update to perfom the move.
   * @return an iterable with variables and their values to update to perfom the move.
   */
  def getValuesToAssign:Iterable[(IntVar,Int)]

}


