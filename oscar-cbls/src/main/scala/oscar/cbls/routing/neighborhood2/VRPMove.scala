package oscar.cbls.routing.neighborhood2

import oscar.cbls.invariants.core.computation.Variable
import oscar.cbls.routing.model.{MoveDescription, VRP}
import oscar.cbls.search.move.Move

abstract class VRPMove(override val objAfter: Int,
                       val vrp: VRP with MoveDescription,
                       override val neighborhoodName:String = null)
  extends Move(objAfter, neighborhoodName) {

  /** to actually take the move */
  override def commit(){
    vrp.cleanRecordedMoves()
    encodeMove()
    vrp.commit(false)
  }

  override def touchedVariables: List[Variable] = {
    vrp.cleanRecordedMoves()
    encodeMove()
    val toReturn = vrp.touchedVariablesByEncodedMove
    vrp.cleanRecordedMoves()
    toReturn
  }

  def encodeMove()
}
