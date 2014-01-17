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

package oscar.cbls.invariants.lib.numeric

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation._

/** sum(i in cond) vars(i)
 * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
 * @param vars is a set of IntVars
 * @param cond is the condition for selecting variables in the set of summed ones, cannot be null
 */
case class SumElements(vars: Array[CBLSIntVar], cond: CBLSSetVar) extends IntInvariant with Bulked[CBLSIntVar, Unit]{
  assert(vars.size > 0, "Invariant SumElements declared with zero vars to max")
  assert(cond != null, "cond cannot be null for SumElements")

  def myMin = Int.MinValue
  def myMax = Int.MaxValue
  var output: CBLSIntVar = null

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.indices.end) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  bulkRegister(vars)

  for(i <- cond.value){
    keyForRemoval(i) = registerDynamicDependency(vars(i),i)
  }
  finishInitialization()

  override def setOutputVar(v: CBLSIntVar) {
      output = v
      //collecter les counts et le max
      output.setDefiningInvariant(this)
      output := cond.value.foldLeft(0)((acc, i) => acc + vars(i).value)
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, index:Int, OldVal: Int, NewVal: Int) {
    //it is always a listened one, but we could check this here
    assert(vars(index)==v)
    assert(keyForRemoval(index)!=null)
    output :+= (NewVal - OldVal)
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval(value) = registerDynamicDependency(vars(value),value)

    output :+= vars(value).value
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)
    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval(value) = null

    output :-= vars(value).value
  }

  override def checkInternals(c:Checker) {
    c.check(output.value == cond.value.foldLeft(0)((acc, i) => acc + vars(i).value),
        Some("output.value == cond.value.foldLeft(0)((acc, i) => acc + vars(i).value)"))
  }
}

/** prod(i in cond) vars(i)
 * This invariant might modify vars array by cloning some variables to ensure that each variable only appears once.
 * @param vars is a set of IntVars
 * @param cond is the condition for selecting variables in the set of summed ones.
 */
case class ProdElements(vars: Array[CBLSIntVar], cond: CBLSSetVar) extends IntInvariant with Bulked[CBLSIntVar, Unit]{
  assert(cond != null, "cond cannot be null for ProdElements")

  def myMin = Int.MinValue
  def myMax = Int.MaxValue
  var output: CBLSIntVar = null

  val keyForRemoval: Array[KeyForElementRemoval] =  Array.fill(vars.length) {null}

  registerStaticDependency(cond)
  registerDeterminingDependency(cond)

  bulkRegister(vars)

  for(i <- cond.value){
    keyForRemoval(i) = registerDynamicDependency(vars(i),i)
  }

  finishInitialization()

  var NullVarCount:Int = 0
  var NonNullProd:Int = 1

  @inline
  private def affectOutput(){
    if (NullVarCount == 0){
      output := NonNullProd
    }else{
      output := 0
    }
  }

  override def setOutputVar(v: CBLSIntVar) {
    output = v
    output.setDefiningInvariant(this)
    NullVarCount = cond.value.count(i => vars(i).value == 0)
    NonNullProd = cond.value.foldLeft(1)((acc,i) => if(vars(i).value == 0){acc}else{acc*vars(i).value})
    affectOutput()
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, index:Int, OldVal: Int, NewVal: Int) {
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
    affectOutput()
  }

  @inline
  override def notifyInsertOn(v: CBLSSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) == null)
    keyForRemoval(value) = registerDynamicDependency(vars(value),value)

    if(vars(value).value == 0){
      NullVarCount += 1
    }else{
      NonNullProd *= vars(value).value
    }
    affectOutput()
  }

  @inline
  override def notifyDeleteOn(v: CBLSSetVar, value: Int) {
    assert(v == cond)
    assert(keyForRemoval(value) != null)

    unregisterDynamicDependency(keyForRemoval(value))
    keyForRemoval(value) = null

    if(vars(value).value == 0){
      NullVarCount -= 1
    }else{
      NonNullProd = NonNullProd / vars(value).value
    }
    affectOutput()
  }

  override def checkInternals(c:Checker) {
    c.check(output.value == cond.value.foldLeft(1)((acc, i) => acc * vars(i).value),
        Some("output.value (" + output.value
            + ") == cond.value.foldLeft(1)((acc, i) => acc * vars(i).value) ("
            + cond.value.foldLeft(1)((acc, i) => acc * vars(i).value) + ")"))
  }
}
