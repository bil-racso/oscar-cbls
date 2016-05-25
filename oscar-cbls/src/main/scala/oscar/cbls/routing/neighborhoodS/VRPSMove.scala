package oscar.cbls.routing.neighborhoodS

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Variable}
import oscar.cbls.routing.model.{HotSpottingInfo, VRP}
import oscar.cbls.search.core.EasyNeighborhood
import oscar.cbls.search.move.Move

abstract class VRPSMove(override val objAfter: Int,
                       val neighborhood: EasyNeighborhood[_],
                       override val neighborhoodName:String = null, vrp:VRPS)
  extends Move(objAfter, neighborhoodName) with HotSpottingInfo{

  override def touchedVariables: List[Variable] = List(vrp.seq)
}

abstract class EasyRoutingSNeighborhood extends EasyNeighborhood{

  def evalObjAndRollBack

}