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
class CPIntervalVarViewOffset(v: CPIntervalVar, offset: Int) extends CPIntervalVar {
  
  final override val store: CPStore = v.store
  
  override val name: String = s"${v.name} + $offset"
    
  final override def transform(v: Int): Int = offset + this.v.transform(v)    
	
	final override def isBound: Boolean = v.isBound
	
	final override def size: Int = v.size
	
  final override def isEmpty: Boolean = v.isEmpty
	
	final override def constraintDegree: Int = v.constraintDegree
	
	final override def isBoundTo(value: Int): Boolean = v.isBoundTo(value-offset)
	
	final override def hasValue(value: Int): Boolean = v.hasValue(value-offset)
	
	final override def valueAfter(value: Int): Int = v.valueAfter(value-offset) + offset
	
	final override def valueBefore(value: Int): Int = v.valueBefore(value-offset) + offset
	
	final override def randomValue(rand: Random): Int = v.randomValue(rand) + offset
	
	final override def updateMin(value: Int): CPOutcome = v.updateMin(value-offset)
	
	final override def assign(value: Int): CPOutcome = v.assign(value-offset)

	final override def updateMax(value: Int): CPOutcome = v.updateMax(value-offset)
	
	final override def min: Int = v.min + offset
	
	final override def max: Int = v.max + offset
	
	final override def iterator: Iterator[Int] = {
		v.iterator.map(_ + offset)
	}
	
	final override def toString(): String = "view with shift "+offset+" on ("+v+")";
		
	final override def callPropagateWhenBind(c: Constraint): Unit = v.callPropagateWhenBind(c)
	
	final override def callPropagateWhenBoundsChange(c: Constraint): Unit = v.callPropagateWhenBoundsChange(c)
		
	// this method is useful when you have a view final override defined on a view
	final override def callValBindWhenBind(c: Constraint, variable: CPIntervalVar): Unit = v.callValBindWhenBind(c, variable)
	
	final override def callValBindWhenBind(c: Constraint): Unit = v.callValBindWhenBind(c,this)
	
	
	// this method is useful when you have a view final override defined on a view
	final override def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar): Unit = v.callUpdateBoundsWhenBoundsChange(c, variable)
	
	final override def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = v.callUpdateBoundsWhenBoundsChange(c,this)
		
	// this method is useful when you have a view final override defined on a view
	final override def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar,idx: Int): Unit = v.callValBindIdxWhenBind(c, variable,idx)
	
	final override def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = v.callValBindIdxWhenBind(c,this,idx)
	
	// this method is useful when you have a view final override defined on a view
	final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);
		
	final override def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = v.callUpdateBoundsIdxWhenBoundsChange(c,this,idx)
}

object CPIntervalVarViewOffset {
  def apply(v: CPIntervalVar, b: Int): CPIntervalVar = {
    new CPIntervalVarViewOffset(v, b)
  }
}

  
