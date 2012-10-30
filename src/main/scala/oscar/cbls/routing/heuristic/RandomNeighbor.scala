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

import oscar.cbls.search.SearchEngine
import scala.util.Random
import oscar.cbls.routing.{HopDistance, VRP}

/**
 * Works for many vehicles.
 */

object RandomNeighbor extends SearchEngine{
  def apply(vrp:VRP with HopDistance){
    val current:Array[Int] = Array.tabulate(vrp.V)(i => i)
    var v = 0

    val random= Random.shuffle((vrp.Next.indices) drop vrp.V)

    for (i <- 0 until vrp.N-vrp.V){
      vrp.Next(current(v)) := random(i)
      current(v) = random(i)
      v = (v+1)%vrp.V
    }
    for (vehicle <- 0 until vrp.V) vrp.Next(current(vehicle)) := vehicle //closing the loop

  }
}
