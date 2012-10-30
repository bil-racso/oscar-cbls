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
  *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
  ******************************************************************************/

import oscar.cbls.search.SearchEngine
import oscar.cbls.routing.{VRP, HopDistance}

/**
 * Works for many vehicles.
 */

object NearestNeighbor extends SearchEngine{
  def apply(vrp:VRP with HopDistance){
    val current:Array[Int] = Array.tabulate(vrp.V)(i => i)
    var v = 0
    var routed :Array[Boolean] = new Array[Boolean](vrp.N)
    for (i <- 0 until vrp.V) routed(i)=true


    for (p <- 0 until vrp.N-vrp.V){
      //initialization: shortest next hop for each vehicle
      val nextForI = selectMin(vrp.Next.indices)(
      (j:Int) => vrp.getHop(current(v),j),
      (j:Int) => (vrp.Next(j).value==j && j>=vrp.V && !routed(j) ))
      println("Le prochain est "+nextForI)
      vrp.Next(current(v)) := nextForI
      current(v) = nextForI
      routed(nextForI) = true
      v = (v+1)%vrp.V
    }
     for (vehicle <- 0 until vrp.V) vrp.Next(current(vehicle)) := vehicle //closing the loop
  }
}
