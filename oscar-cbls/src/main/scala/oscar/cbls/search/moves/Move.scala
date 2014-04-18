package oscar.cbls.search.moves

import oscar.cbls.invariants.core.computation.CBLSIntVar

abstract class StatelessNeighborhood extends Neighborhood{
  //this resets the internal state of the move combinators
  override def reset(){}
}

abstract class Move(val objAfter:Int){
  def comit()
}

/** a neighborhood that never finds any move
  */
class NoMove extends StatelessNeighborhood{
  override def getImprovingMove(): Option[Move] = None
}

case class AssingMove(i:CBLSIntVar,v:Int, override val objAfter:Int) extends Move(objAfter){
  override def comit() {i := v}
}

case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, override val objAfter:Int) extends Move(objAfter){
  override def comit() {i :=: j}
}

