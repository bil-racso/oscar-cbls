/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.des.engine


import scala.collection.mutable._
import scala.util.continuations._

/**
 * Capacitated resource where waiting customers are served in FIFO order
 * @author pschaus
 */
class Resource(m : Model, capacity: Int) {
	
	private var n = 0
	private val pendings = new DoubleLinkedList()
	
	def request(): Unit @suspendable = {
		if (n < capacity) {
	         n += 1
	    } else {
	         shift {
	            k: (Unit=>Unit)=>
	         	val a = pendings :+ {k()}
	         }
	    }
	}
	
	def release() {
		n -= 1
		if (pendings.nonEmpty) {
			val block = pendings.head
			pendings.drop(1)
			block
		}
	}
	
}

class UnaryResource(m : Model) extends Resource(m,1)