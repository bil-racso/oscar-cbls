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


package oscar.cbls.invariants.lib.minmax

import oscar.cbls.invariants.core.computation.{IntVar, IntInvariant, SetVar}
import oscar.cbls.invariants.core.propagation.Checker

//Log
abstract class MiaxSet(v: SetVar) extends IntInvariant{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def myMax = v.getMaxVal
  def myMin = v.getMinVal

  def Better(a: Int, b:Int): Boolean

  var output: IntVar = null

  def Default: Int

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    performPropagation()
  }

  def name: String

  var wasEmpty:Boolean = v.value.isEmpty

  @inline
  override def notifyInsertOn(v: SetVar, value: Int) {
    if (wasEmpty){
      output := value
    }else if(!this.isScheduled && Better(value,output.getValue(true))){
      output := value
    }
    wasEmpty = false
  }

  @inline
  override def notifyDeleteOn(v: SetVar, value: Int) {
    if (v.value.isEmpty){ //TODO: avoid querying this directly on the intsetvar!
      wasEmpty = true
      output := Default
    } else if(!this.isScheduled && value == output.getValue(true)){
      scheduleForPropagation()
    }
  }

  override def performPropagation(){
    throw new Exception("you must override this to set the output because it has been lost")
  }
}

/** maintains output = Min(v)
 * where
 * * output is an IntVar
 * * v is an IntSetVar
 * @param Default is the default value if v is empty
 * update is O(log(n))
 * */
case class MinSet(v: SetVar, Default: Int = Int.MaxValue) extends MiaxSet(v) {
  override def name = "MinSet"

  override def Better(a:Int,b:Int):Boolean = a < b

  override def performPropagation(){
    if (v.value.isEmpty){
      output := Default
    }else{
      output := v.value.firstKey
    }
  }

  override def checkInternals(c:Checker){
    if (v.value.isEmpty){
      c.check(output.value == Default, Some("output.value == Default"))
    }else{
      c.check(output.value == v.value.foldLeft(Int.MaxValue)((acc,value) => if (acc > value) value else acc),
          Some("output.value == v.value.foldLeft(Int.MaxValue)((acc,value) => if (acc > value) value else acc)"))
    }
  }
}

/** maintains output = Max(v)
 * where
 * * output is an IntVar
 * * v is an IntSetVar
 * @param Default is the default value if v is empty
 * update is O(log(n))
 * */
case class MaxSet(v: SetVar, Default: Int = Int.MinValue) extends MiaxSet(v) {
  override def name = "MaxSet"

  override def Better(a:Int,b:Int):Boolean = a > b

  override def performPropagation(){
    if (v.value.isEmpty){
      output := Default
    }else{
      output := v.value.lastKey
    }
  }

  override def checkInternals(c:Checker){
    if (v.value.isEmpty){
      c.check(output.value == Default, Some("output.value == Default"))
    }else{
      c.check(output.value == v.value.foldLeft(Int.MinValue)((acc,value) => if (acc < value) value else acc),
          Some("output.value == v.value.foldLeft(Int.MinValue)((acc,value) => if (acc < value) value else acc)"))
    }
  }
}
