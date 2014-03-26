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
package oscar.cbls.search.binPacking

import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar}
import oscar.cbls.objective.Objective
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.search.moves.Neighborhood

case class Item(number:Int,
                size:Int,
                bin: CBLSIntVar)

case class Bin(number:Int,
               size:Int,
               var items:CBLSSetVar = null,
               var violation:CBLSIntVar = null)

case class BinPackingProblem(items:Map[Int,Item],
                             bins: Map[Int,Bin],
                             overallViolation:Objective,
                             mostViolatedBins:CBLSSetVar)