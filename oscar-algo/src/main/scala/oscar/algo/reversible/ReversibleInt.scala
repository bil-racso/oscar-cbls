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
package oscar.algo.reversible;


/**
 * Creates Reversible integer
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleInt(node: ReversibleNode ,value : Int) extends ReversiblePointer[Int](node,value) {

    /**
     * Creates a reversible Int initialized to 0
     * @param node
     */
	def this(node: ReversibleNode) = this(node,0)

    /**
     * increment the reversible integer by one
     */
	def incr(): Int = {
		assert(hasValue())
		val v = getValue() + 1
		setValue(v)
		v
	}
	
	// I have to add it for Java compatibility
	override def getValue(): Int = super.getValue()

	
	def +=(i: Int): Unit = {
	  val v: Int = getValue() + i
	  setValue(v)
	}

    /**
     * decrement the reversible integer by one
     */
	def decr(): Int = {
		assert(hasValue())
		val v = getValue() - 1
		setValue(v)
		v
	}

}