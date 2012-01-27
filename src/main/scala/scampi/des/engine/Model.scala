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
 * This is the main engine of the simulation.
 * Every Process in the simulation should wait, require resource ... on an instance of this class.
 * @author pschaus
 */
class Model {
   
	private val eventQueue = new PriorityQueue[SimEvent]()
	private var currentTime = 0.0
//	
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
			else{
				currentTime = horizon;
				return
			}
		}
	}

	def time(o: Any): Double = {
	  clock()
	}
	def frequency[_](state: State[_]) = new Frequency(this,state)
	
	def waitt(duration : Double)(block : => Unit):Unit =  {
		assert(duration >= 0)
		addEvent(new WaitEvent(clock + duration, block))
	}
	
    def waitt(duration : Int)(block : => Unit) {
		waitt(duration.toDouble)(block)
	}
    
    def wait(duration : Double):Unit@suspendable= {
		shift{ k:(Unit=>Unit) =>
		  waitt(duration.toDouble){k()}
		}
    }
    def wait(duration : Int):Unit@suspendable={wait(duration.toDouble)}
	
	def request(r : Resource): Unit @ suspendable = {
		r.request
	}

	def release(r : Resource) {
		r.release()
	}
//	
	def suspend(proc : Process):Unit @suspendable = {proc.suspend()}

	def resume(proc : Process){
		proc.resume()
	}

}

object Model{
  def main(args: Array[String]){
    println(45)
  }
}