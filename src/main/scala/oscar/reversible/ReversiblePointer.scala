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

package oscar.reversible;


/**
 * Creates a generic reversible pointer
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversiblePointer[T](node: ReversibleSearchNode, v: T) extends Reversible(node) {
  
    //this(node: ReversibleSearchNode) = this(node,null)
    
	
	var pointer: Option[T] = Some(v)


	
	def setValue(value: T) {
		if (value != pointer) {
			trail()
			this.pointer = Some(value)
		}
	}
	
	def value_=(value: T) {
        setValue(value)
    }

    /**
     * Check if the pointer is different from null
     * @return true if the pointer is != null, false otherwise
     */
	def hasValue() = pointer.get != null
	

    /**
     *
     * @return the current pointer
     */
	def getValue(): T = pointer.getOrElse(v)
	
	
	def value = getValue
		
	override def addOnTrail() {
		node.getTrail().addEntry(this, pointer.getOrElse(v))
	}

	override def restore(value: Object) {
		pointer = null
		pointer = Some(value.asInstanceOf[T])
	}
	
	override def toString() = pointer.getOrElse("")+""
	

}
