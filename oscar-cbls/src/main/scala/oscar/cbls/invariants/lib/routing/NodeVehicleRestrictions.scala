package oscar.cbls.invariants.lib.routing
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
