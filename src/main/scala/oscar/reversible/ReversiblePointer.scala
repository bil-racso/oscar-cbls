/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.reversible;

import oscar.reversible.Reversible;

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
