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

import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar}

abstract class StatelessNeighborhood extends Neighborhood{
  //this resets the internal state of the move combinators
  final override def reset(){}

  override def toString: String = this.getClass.getSimpleName
}

/** a neighborhood that never finds any move (quite useless, actually)
  */
case class NoMoveNeighborhood() extends StatelessNeighborhood{
  override def getImprovingMove(): SearchResult = NoMoveFound
}

case class AssignMove(i:CBLSIntVar,v:Int, override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  override def commit() {i := v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
    "AssignMove(" + i + " set to " + v + " objAfter:" + objAfter + ")"
  }
}

case class AddToSetMove(s:CBLSSetVar,v:Int, override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  override def commit() {s :+= v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "AddToSetMove(" + s + " :+= " + v + " objAfter:" + objAfter + ")"
  }
}

case class RemoveFromSetMove(s:CBLSSetVar,v:Int, override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  override def commit() {s :-= v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "RemoveFromSetMove(" + s + " :-= " + v + " objAfter:" + objAfter + ")"
  }
}

case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  override def commit() {i :=: j}

  override def toString: String  = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
    "SwapMove(" + i + " swapped with " + j + " objAfter:" + objAfter + ")"
  }
}

case class CompositeMove(ml:List[Move], override val objAfter:Int, neighborhoodName:String = null) extends Move(objAfter){
  def this(ml:List[Move]){
    this(ml, ml.last.objAfter)
  }

  override def commit() {
    for(m <- ml) m.commit()
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
  def commit(){
    callBack()
    initialMove.commit
  }

  override def toString: String = initialMove.toString
}