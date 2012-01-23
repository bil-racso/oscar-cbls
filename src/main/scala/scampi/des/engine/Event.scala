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

/**
 * Objects stored in the main queue of the simulation. The modeler should not have knowledge of it.
 * @author pschaus
 */
abstract class SimEvent(val time: Double) extends Ordered[SimEvent] {
	def process = {}
	
	def compare(that : SimEvent) = that.time.compare(this.time)
	
	
}

class WaitEvent(time: Double, block: => Unit ) extends SimEvent(time) {
	
	override def process = block
	
}




