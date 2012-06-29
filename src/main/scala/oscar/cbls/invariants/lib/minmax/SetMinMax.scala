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
 ******************************************************************************/
/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.invariants.lib.minmax

import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.computation.{IntVar, IntInvariant, IntSetVar}
import oscar.cbls.invariants.core.propagation.PropagationElement._

//Log
abstract case class MiaxSet(v: IntSetVar) extends IntInvariant{

  registerStaticAndDynamicDependency(v)
  finishInitialization()

  def MyMax = v.getMaxVal
  def MyMin = v.getMinVal

  def Better(a: Int, b:Int): Boolean

  var output: IntVar = null

  def Default: Int

  override def setOutputVar(v: IntVar) {
    output = v.asInstanceOf[IntVar]
    output.setDefiningInvariant(this)
    performPropagation()
  }

  def name: String

  var wasEmpty:Boolean = v.isEmpty

  @inline
  override def notifyInsertOn(v: IntSetVar, value: Int) {
    if (wasEmpty){
      output := value
    }else if(!this.isScheduled && Better(value,output.getValue(true))){
      output := value
    }
    wasEmpty = false
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, value: Int) {
    if (v.getValue().isEmpty){ //TODO: avoid querying this directly on the intsetvar!
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
case class MinSet(override val v: IntSetVar, Default: Int = Int.MaxValue) extends MiaxSet(v) {
  override def name = "MinSet"

  override def Better(a:Int,b:Int):Boolean = a < b

  override def performPropagation(){
    if (v.isEmpty){
      output := Default
    }else{
      output := v.getValue().firstKey
    }
  }

  override def checkInternals(){
    if (v.getValue().isEmpty){
      assert(output.getValue() == Default)
    }else{
      assert(output.getValue() == v.getValue().foldLeft(Int.MaxValue)((acc,value) => if (acc > value) value else acc))
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
case class MaxSet(override val v: IntSetVar, Default: Int = Int.MinValue) extends MiaxSet(v) {
  override def name = "MaxSet"

  override def Better(a:Int,b:Int):Boolean = a > b

  override def performPropagation(){
    if (v.isEmpty){
      output := Default
    }else{
      output := v.getValue().lastKey
    }
  }

  override def checkInternals(){
    if (v.getValue().isEmpty){
      assert(output.getValue() == Default)
    }else{
      assert(output.getValue() == v.getValue().foldLeft(Int.MinValue)((acc,value) => if (acc < value) value else acc))
    }
  }
}
