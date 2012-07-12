/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.invariants.lib.numeric

import oscar.cbls.invariants.core.computation._;
import oscar.cbls.invariants.core.propagation._;
import oscar.cbls.invariants.core.computation.Invariant._
import collection.immutable.{SortedMap, SortedSet}
import oscar.cbls.invariants.core.computation.IntVar._

/** sum(i in cond) vars(i)
 * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
 * @param vars is a set of IntVars
 * @param cond is the condition for selecting variables in the set of summed ones, cannot be null
 */
case class SumElements(var vars: Array[IntVar], cond: IntSetVar) extends IntInvariant with Bulked[IntVar, Unit]{
  assert(vars.size > 0, "Invariant SumElements declared with zero vars to max")
  assert(cond != null, "cond cannot be null for SumElements")

  def MyMin = Int.MinValue
  def MyMax = Int.MaxValue
  var output: IntVar = null

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.indices.end) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  if(vars != null){
    for(v <- vars)registerStaticDependency(v)
    BulkLoad(vars, null)
  }

  finishInitialization()

  override def BulkLoad(bulkedVar: Array[IntVar], bcr: Unit){
    vars = bulkedVar
    for(i <- cond.getValue()){
      keyForRemoval.update(i, registerDynamicDependency(vars(i),i))
    }
  }

  override def setOutputVar(v: IntVar) {
      output = v
      //collecter les counts et le max
      output.setDefiningInvariant(this)
      output := cond.getValue().foldLeft(0)((acc, i) => acc + vars(i))
  }

  @inline
  override def notifyIntChanged(v: IntVar, index:Int, OldVal: Int, NewVal: Int) {
    //it is always a listened one, but we could check this here
    assert(vars(index)==v)
    assert(keyForRemoval(index)!=null)
    output := output.getValue() - OldVal + NewVal
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval.update(value, registerDynamicDependency(vars(value),value))

    output := output.getValue() + vars(value).getValue()
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)
    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval.update(value, null)

    output := output.getValue() - vars(value).getValue()
  }

  override def checkInternals() {
    assert(output.getValue() == cond.getValue().foldLeft(0)((acc, i) => acc + vars(i)))
  }
}

/** prod(i in cond) vars(i)
 * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
 * @param vars is a set of IntVars
 * @param cond is the condition for selecting variables in the set of summed ones.
 */
case class ProdElements(var vars: Array[IntVar], cond: IntSetVar) extends IntInvariant with Bulked[IntVar, Unit]{
  assert(cond != null, "cond cannot be null for ProdElements")

  def MyMin = Int.MinValue
  def MyMax = Int.MaxValue
  var output: IntVar = null

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.indices.end) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  if(vars != null){
    for(v <- vars)registerStaticDependency(v)
    BulkLoad(vars, null)
  }

  finishInitialization()
  var NullVarCount:Int = 0
  var NonNullProd:Int = 0

  override def BulkLoad(bulkedVar: Array[IntVar], bcr: Unit){
    vars = bulkedVar
    for(i <- cond.getValue()){
      keyForRemoval.update(i, registerDynamicDependency(vars(i),i))
    }
  }

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    NullVarCount = cond.getValue().count(i => vars(i).getValue() == 0)
    NonNullProd = cond.getValue().foldLeft(1)((acc,i) => if(vars(i).getValue() == 0){acc}else{acc*vars(i).getValue()})
    if (NullVarCount != 0){
      output := 0
    }else{
      output := NonNullProd
    }
  }

  @inline
  override def notifyIntChanged(v: IntVar, index:Int, OldVal: Int, NewVal: Int) {
    //it is always a listened one, but we could check this here
    assert(vars(index) == v)
    assert(keyForRemoval(index)!=null)
    if (OldVal == 0 && NewVal != 0){
      NullVarCount -=1
      NonNullProd *=NewVal
    }else if(OldVal != 0 && NewVal == 0){
      NullVarCount +=1
      NonNullProd =NonNullProd/OldVal
    }else{
      NonNullProd = NonNullProd/OldVal
      NonNullProd = NonNullProd * NewVal
    }
    if (NullVarCount == 0){
      output := NonNullProd
    }else{
      output := 0
    }
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval.update(value, registerDynamicDependency(vars(value),value))

    if(vars(value).getValue() == 0){
      NullVarCount +=1
    }else{
      NonNullProd *=vars(value).getValue()
    }
    if (NullVarCount == 0){
      output := NonNullProd
    }else{
      output := 0
    }
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)

    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval.update(value, null)

    if(vars(value).getValue() == 0){
      NullVarCount -=1
    }else{
      NonNullProd =NonNullProd / vars(value).getValue()
    }
    if (NullVarCount == 0){
      output := NonNullProd
    }else{
      output := 0
    }
  }

  override def checkInternals() {
    assert(output.getValue() == cond.getValue().foldLeft(1)((acc, i) => acc * vars(i)))
  }
}
