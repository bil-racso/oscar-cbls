/**
 * Created with IntelliJ IDEA.
 * User: Florent
 * Date: 10/11/12
 * Time: 15:07
 * To change this template use File | Settings | File Templates.
 */

package oscar.cbls.routing.heuristic

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

import oscar.cbls.routing._
import neighborhood.{ReinsertPoint, Neighbor}

/**
 * Works for many vehicles.
 */

object Unrouted extends Heuristic{

  def addPoint {}
  def addPoints = addPoint
  def start {}

  def apply(vrp:VRP with ObjectiveFunction with PenaltyForUnrouted with PositionInRouteAndRouteNr
    with HopDistance){

    val current:Array[Neighbor] = Array.tabulate(vrp.V)(_ => null)
    for (v <- 0 until vrp.V)
      vrp.Next(v) := v
    for (p <- vrp.V until vrp.N)
      vrp.Next(p) := vrp.N
    vrp.m.propagate()

    heuristicTimer.setPercentComplete(100)
    heuristicTimer.unlock
  }

}
