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

/**
 * This is the main engine of the simulation.
 * Every Process in the simulation should wait, require resource ... on an instance of this class.
 * @author pschaus
 */
class Model {
   
	private val eventQueue = new PriorityQueue[SimEvent]()
	private var currentTime = 0.0
	
	def clock() : Double = currentTime
	
	private def addEvent(e : SimEvent) = eventQueue += e
	
	def simulate(horizon: Int,verbose: Boolean = true) {
		while (eventQueue.nonEmpty) {
			val e = eventQueue.dequeue()
			if(verbose && e.time <= horizon && e.time != currentTime){
				println("-----------> time: "+  e.time)
			}
			currentTime = e.time;
			if(currentTime <= horizon){
				e.process
			}
			else {
				currentTime = horizon;
				return
			}
		}
	}

	def wait(duration : Double)(block : => Unit) {
		assert(duration >= 0)
		addEvent(new WaitEvent(clock + duration, block))
	}
	
    def wait(duration : Int)(block : => Unit) {
		wait(duration.toDouble)(block)
	}
	
	def request(r : Resource)(block : => Unit) {
		r.request(block)
	}

	def release(r : Resource) {
		r.release()
	}
	
	def suspend(proc : Process)(block : => Unit) {
		proc.suspend(block)
	}

	def resume(proc : Process){
		proc.resume()
	}

}