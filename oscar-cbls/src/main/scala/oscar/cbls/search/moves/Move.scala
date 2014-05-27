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

package oscar.cbls.search.moves

import oscar.cbls.invariants.core.computation.CBLSIntVar


abstract class StatelessNeighborhood extends Neighborhood{
  //this resets the internal state of the move combinators
  override def reset(){}
}

/** a neighborhood that never finds any move (quite useless, actually)
  */
case class NoMoveNeighborhood() extends StatelessNeighborhood{
  override def getImprovingMove(): SearchResult = NoMoveFound
}

case class AssingMove(i:CBLSIntVar,v:Int, override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  override def comit() {i := v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
    "AssingMove(" + i + " set to " + v + " objAfter:" + objAfter + ")"
  }
}

case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  override def comit() {i :=: j}

  override def toString: String  = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
    "SwapMove(" + i + " swapped with " + j + " objAfter:" + objAfter + ")"
  }
}

case class CompositeMove(ml:List[Move], override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  def this(ml:List[Move]){
    this(ml, ml.last.objAfter)
  }

  override def comit() {
    for(m <- ml) m.comit()
  }

  override def toString: String  = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
    "CompositeMove(" + ml.mkString(" and ") + " objAfter:" + objAfter + ")"
  }
}

object CallBackMove{
  def apply(initialMove:Move, callBack: => Unit): CallBackMove = new CallBackMove(initialMove, (_:Unit) => callBack)
}

case class CallBackMove(initialMove:Move, callBack: Unit => Unit) extends Move(initialMove.objAfter){
  def comit(){
    callBack()
    initialMove.comit
  }

  override def toString: String = initialMove.toString + " (with callBack)"
}