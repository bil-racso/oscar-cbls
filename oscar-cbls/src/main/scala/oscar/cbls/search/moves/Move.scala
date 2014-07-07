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


/** standard move template
  *
  * @param objAfter the objective after this assignation will be performed
  * @author renaud.delandtsheer@cetic.be
  */
abstract class Move(val objAfter:Int){
  /**to actually take the move*/
  def commit()
}

/** standard move that assigns an int value to a CBLSIntVar
  *
  * @param i the variable
  * @param v the value to assign
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class AssignMove(i:CBLSIntVar,v:Int, override val objAfter:Int, neighborhoodName:String = null)
  extends Move(objAfter){

  override def commit() {i := v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "AssignMove(" + i + " set to " + v + " objAfter:" + objAfter + ")"
  }
}

/** standard move that swaps the value of two CBLSIntVar
  *
  * @param i the variable
  * @param j the other variable
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class SwapMove(i:CBLSIntVar,j:CBLSIntVar, override val objAfter:Int, neighborhoodName:String = null)
  extends Move(objAfter){

  override def commit() {i :=: j}

  override def toString: String  = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "SwapMove(" + i + " swapped with " + j + " objAfter:" + objAfter + ")"
  }
}

/** standard move that adds a value to a CBLSSEtVar
  *
  * @param s the variable
  * @param v the value to add to the set
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class AddToSetMove(s:CBLSSetVar,v:Int, override val objAfter:Int, neighborhoodName:String = null)
  extends Move(objAfter){

  override def commit() {s :+= v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "AddToSetMove(" + s + " :+= " + v + " objAfter:" + objAfter + ")"
  }
}

/** standard move that removes a value to a CBLSSEtVar
  *
  * @param s the variable
  * @param v the value to remove to the set
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class RemoveFromSetMove(s:CBLSSetVar,v:Int, override val objAfter:Int, neighborhoodName:String = null)
  extends Move(objAfter){

  override def commit() {s :-= v}

  override def toString: String = {
    (if (neighborhoodName != null) neighborhoodName + ": " else "") +
      "RemoveFromSetMove(" + s + " :-= " + v + " objAfter:" + objAfter + ")"
  }
}

/** a composition of a list of moves; the move will be taken in the order given by the list
  *
  * @param ml the list of moves constituting this one
  * @param objAfter the objective after this assignation will be performed
  * @param neighborhoodName a string describing the neighborhood hat found the move (for debug purposes)
  * @author renaud.delandtsheer@cetic.be
  */
case class CompositeMove(ml:List[Move], override val objAfter:Int, neighborhoodName:String = null)
  extends Move(objAfter){

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

/** an instrumented move that performs a callBack before being taken
  * the move itself is given in parameter.
  *
  * @param initialMove the actual move
  * @param callBack the method to invoke before the actual move is taken
  * @author renaud.delandtsheer@cetic.be
  */
case class CallBackMove(initialMove:Move, callBack: () => Unit) extends Move(initialMove.objAfter){
  def commit(){
    callBack()
    initialMove.commit()
  }

  override def toString: String = initialMove.toString
}
