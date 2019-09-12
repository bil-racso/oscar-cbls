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


package oscar.cbls.lib.invariant.minmax

import oscar.cbls._
import oscar.cbls.core._

import scala.collection.immutable.SortedSet

//Log
abstract class MiaxSet(v: SetValue)
  extends IntInvariant(initialDomain = v.min to v.max)
  with SetNotificationTarget{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def Better(a: Int, b:Int): Boolean

  def Default: Int

  performPropagation()

  var wasEmpty:Boolean = v.value.isEmpty

  override def notifySetChanges(v: ChangingSetValue, d: Int, addedValues: Iterable[Int], removedValues: Iterable[Int], oldValue: SortedSet[Int], newValue: SortedSet[Int]) : Unit = {
    for (added <- addedValues) notifyInsertOn(v: ChangingSetValue, added)
    for(deleted <- removedValues) notifyDeleteOn(v: ChangingSetValue, deleted)
  }

  @inline
  def notifyInsertOn(v: ChangingSetValue, value: Int) {
    if (wasEmpty){
      this := value
    }else if(!this.isScheduled && Better(value,this.newValue)){
      this := value
    }
    wasEmpty = false
  }

  @inline
  def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    if (v.value.isEmpty){ //TODO: avoid querying this directly on the intsetvar!
      wasEmpty = true
      this := Default
    } else if(!this.isScheduled && value == this.newValue){
      scheduleForPropagation()
    }
  }

  override def performInvariantPropagation(){
    throw new Exception("you must override this to set the this because it has been lost")
  }
}

/** maintains output = Min(v)
  * where
  * * output is an IntVar
  * * v is an IntSetVar
  * @param Default is the default value if v is empty
  * update is O(log(n))
  * @author renaud.delandtsheer@cetic.be
  */
case class MinSet(v: SetValue, Default: Int = Int.MaxValue) extends MiaxSet(v) {

  override def Better(a:Int,b:Int):Boolean = a < b

  override def performInvariantPropagation(){
    if (v.value.isEmpty){
      this := Default
    }else{
      this := v.value.firstKey
    }
  }

  override def checkInternals(c:Checker){
    if (v.value.isEmpty){
      c.check(this.value == Default, Some("this.value == Default"))
    }else{
      c.check(this.value == v.value.foldLeft(Int.MaxValue)((acc,value) => if (acc > value) value else acc),
        Some("this.value == v.value.foldLeft(Int.MaxValue)((acc,value) => if (acc > value) value else acc)"))
    }
  }
}

/** maintains output = Max(v)
  * where
  * * output is an IntVar
  * * v is an IntSetVar
  * @param Default is the default value if v is empty
  * update is O(log(n))
  * @author renaud.delandtsheer@cetic.be
  * */
case class MaxSet(v: SetValue, Default: Int = Int.MinValue) extends MiaxSet(v) {

  override def Better(a:Int,b:Int):Boolean = a > b

  override def performInvariantPropagation(){
    if (v.value.isEmpty){
      this := Default
    }else{
      this := v.value.lastKey
    }
  }

  override def checkInternals(c:Checker){
    if (v.value.isEmpty){
      c.check(this.value == Default, Some("this.value == Default"))
    }else{
      c.check(this.value == v.value.foldLeft(Int.MinValue)((acc,value) => if (acc < value) value else acc),
        Some("this.value == v.value.foldLeft(Int.MinValue)((acc,value) => if (acc < value) value else acc)"))
    }
  }
}
