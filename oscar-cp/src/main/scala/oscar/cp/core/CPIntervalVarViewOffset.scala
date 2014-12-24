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
package oscar.cp.core;

import scala.util.Random

/**
 * Represents a view on variable applying an offset on it.
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPIntervalVarViewOffset(v: CPIntervalVar,val b: Int) extends CPIntervalVar(v.store) {
  def transform(v: Int) = b + this.v.transform(v)    
	
	def isBound = v.isBound
	
	override def size = v.size
	
	override def isEmpty = v.isEmpty
	
	def constraintDegree = v.constraintDegree
	
	def isBoundTo(value: Int): Boolean = v.isBoundTo(value-b)
	
	def hasValue(value: Int): Boolean = v.hasValue(value-b)
	
	def valueAfter(value: Int): Int = v.valueAfter(value-b) + b
	
	def valueBefore(value: Int): Int = v.valueBefore(value-b) + b
	
	def randomValue(rand: Random): Int = v.randomValue(rand) + b
	
	def updateMin(value: Int) = v.updateMin(value-b)
	
	def assign(value: Int) = v.assign(value-b)

	def updateMax(value: Int) = v.updateMax(value-b)
	
	
	def min = v.min + b
	
	def max = v.max + b
	
	def iterator = {
		v.iterator.map(_ + b)
	}
	
	override def toString() = "view with shift "+b+" on ("+v+")";
		
	def callPropagateWhenBind(c: Constraint) = v.callPropagateWhenBind(c)
	
	def callPropagateWhenBoundsChange(c: Constraint) = v.callPropagateWhenBoundsChange(c)
		
	// this method is useful when you have a view defined on a view
	def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) = v.callValBindWhenBind(c, variable)
	
	def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c,this)
	
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) = v.callUpdateBoundsWhenBoundsChange(c, variable)
	
	def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c,this)
		
	// this method is useful when you have a view defined on a view
	def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar,idx: Int) = v.callValBindIdxWhenBind(c, variable,idx)
	
	def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c,this,idx)
	
	// this method is useful when you have a view defined on a view
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);
		
	def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx)
}

object CPIntervalVarViewOffset {
  def apply(v: CPIntervalVar, b: Int): CPIntervalVar = {
    new CPIntervalVarViewOffset(v, b)
  }
}

  
