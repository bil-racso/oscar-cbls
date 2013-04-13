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
package oscar.reversible

import oscar.reversible.ReversibleSearchNode

/**
 * Reversible integer
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleInt(node : ReversibleSearchNode, value : Int ) extends ReversiblePointer[Integer](node, value) 
{
	def this(node : ReversibleSearchNode) = this(node,0)
    

    /**
     * increment the reversible integer by one
     */
	def incr() : Int = {
		this += 1
	}

    /**
     * decrement the reversible integer by one
     */
	def decr() : Int = {
		this += -1
	}
	
	/**
	 *  increment the reversible integer by v
	 */
	def +=(v:Int) : Int ={
		assert(hasValue())
		val tmpv = getValue() + v
		setValue(tmpv)
		tmpv
	}
	
	/**
	 *  decrement the reversible integer by v
	 */
	def -=(v:Int) : Int ={
		+=(-v)
	}

}
