package oscar.cbls.invariants.lib.routing

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

import oscar.cbls.invariants.core.computation.ChangingSeqValue

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.CBLSSetVar
import oscar.cbls.invariants.core.computation.ChangingSeqValue


/**
 * Created by rdl on 13-06-16.
 */
class NodeVehicleRestrictions(seq:ChangingSeqValue,
                              v:Int,
                              nodeVehicleRestrictions:List[(Int,Int)],
                              valuesInvolvedInViolation:CBLSSetVar,
                              violation:CBLSIntVar) {

//internal:
//  array node n => SortedMap vehicle v => number of node from start of vehicle reaching n that cannot be reached by vehicle v
}
