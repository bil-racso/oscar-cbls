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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/


package oscar.cbls.search

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.search.moves.{Move, Neighborhood}

/**generic search procedure
  * selects most violated variable, and assigns value that minimizes overall violation
  *  * @author renaud.delandtsheer@cetic.be
  * */
object conflictSearch extends SearchEngine{
  /**Performs a conflict search on the constraint system
    * selects most violated variable, and assigns value that minimizes overall violation
    *
    * Beware: the constraint system can only constraint IntVar, no other types of variables are tolerated
    * @param c the constraint system: the searched variables sill be the ones that are constrained by it
    * @param MaxIt the maximal number of iterations
    * @author renaud.delandtsheer@cetic.be
    * THIS IS EXPERIMENTAL
    */
  def apply(c:ConstraintSystem, MaxIt: Int) {
    var it = 0
    val Variables:Array[CBLSIntVar] = c.constrainedVariables.asInstanceOf[Iterable[CBLSIntVar]].toArray
    val Violations:Array[CBLSIntVar] = Variables.clone().map(c.violation(_))
    while (!c.isTrue && it < MaxIt) {
      val MaxViolVarID = selectMax(Variables.indices,Violations(_:Int).value)
      val NewVal = selectMin(Variables(MaxViolVarID).domain)(c.assignVal(Variables(MaxViolVarID),_:Int),i => true)
      Variables(MaxViolVarID) := NewVal
      it = it + 1
    }
  }
}


class conflictMove(c:ConstraintSystem) extends Neighborhood with SearchEngineTrait{
  val Variables:Array[CBLSIntVar] = c.constrainedVariables.asInstanceOf[Iterable[CBLSIntVar]].toArray
  val Violations:Array[CBLSIntVar] = Variables.clone().map(c.violation(_))
  override def getFirstImprovingMove(): Option[Move] = {
    val oldObj = c.ObjectiveVar.value
    val MaxViolVarID = selectMax(Variables.indices,Violations(_:Int).value)
    val NewVal = selectMin(Variables(MaxViolVarID).domain)(c.assignVal(Variables(MaxViolVarID),_:Int))

    val objAfter = c.assignVal(Variables(MaxViolVarID),NewVal)

    if(objAfter < oldObj){
      Some(new AssingMove(Variables(MaxViolVarID),NewVal,objAfter))
    }else{
      None
    }
  }
}

class conflictMoveFirstImprove(c:ConstraintSystem) extends Neighborhood with SearchEngineTrait{
  val Variables:Array[CBLSIntVar] = c.constrainedVariables.asInstanceOf[Iterable[CBLSIntVar]].toArray
  val Violations:Array[CBLSIntVar] = Variables.clone().map(c.violation(_))
  override def getFirstImprovingMove(): Option[Move] = {
    val oldObj = c.ObjectiveVar.value
    val MaxViolVarID = selectMax(Variables.indices,Violations(_:Int).value)
    selectFirstDo(Variables(MaxViolVarID).domain,(newVal:Int) => c.assignVal(Variables(MaxViolVarID),newVal) < oldObj)(
    (newVal:Int) => {
      val objAfter = c.assignVal(Variables(MaxViolVarID),newVal)
      return Some(new AssingMove(Variables(MaxViolVarID),newVal,objAfter))}
    ,{ null })
    None
  }
}

class AssingMove(i:CBLSIntVar,v:Int, override val objAfter:Int) extends Move(objAfter){
  override def comit() {i := v}
}
