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

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.initialSolution

import oscar.cbls.routing.model._

/**
 * The simplest initial solution consists to start from all unrouted points.
 *
 * Info : This heuristic does not take into account the possible constraints
 * the problem, such as a number of unrouted maximum node.
 */
object AllUnrouted extends Heuristic{
  /**
    * It applies the initial solution to a given vrp problem.
    * @param vrp : the vrp problem that we want to apply the initial solution.
    */
  def apply(vrp:VRP with ObjectiveFunction with Unrouted with PositionInRouteAndRouteNr
    with HopDistance){

    for (v <- 0 until vrp.V)
      vrp.Next(v) := v
    for (p <- vrp.V until vrp.N)
      vrp.Next(p) := vrp.N
    vrp.m.propagate()

    // update the timer (linked to progressBar)
    heuristicTimer.setPercentComplete(100)
  }

}
