package oscar.cbls.routing.seq.neighborhood

import oscar.cbls.invariants.core.computation.Variable
import oscar.cbls.routing.model.HotSpottingInfo
import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.core.EasyNeighborhood
import oscar.cbls.search.move.Move

abstract class VRPSMove(override val objAfter: Int,
                       val neighborhood: EasyNeighborhood[_],
                       override val neighborhoodName:String = null, vrp:VRP)
  extends Move(objAfter, neighborhoodName) with HotSpottingInfo{

  override def touchedVariables: List[Variable] = List(vrp.seq)
}

abstract class EasyRoutingSNeighborhood extends EasyNeighborhood{

  def evalObjAndRollBack

}