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

package oscar.des.engine


import scala.collection.mutable._
import scala.util.continuations._
import oscar.invariants._

/**
 * Capacitated resource where waiting customers are served in FIFO order
 * @author Pierre Schaus, Sebastien Mouthuy
 */

class Resource(var capacity: Int)(implicit m: Model[_]) {
	
	private var n = 0
	private val rel = new EventOne[Unit]
	
	def request[T]() = {
	  
	  val res = if ( n >= capacity ) waitFor[Unit,T](rel)
	  
	  n += 1
	}
	  
	 	
	def release() ={
	  n -= 1
	  if ( n < capacity ) rel.emit()
	}
	def setCapacity(c: Int){
	  for ( i <- capacity until c){
	    rel.emit()
	  }
	  capacity = c
	}
}

object Resource{
  def unary(implicit m: Model[_]) = new Resource(1)(m)
  def apply(cap: Int)(implicit m: Model[_]) = new Resource(cap)(m)
}