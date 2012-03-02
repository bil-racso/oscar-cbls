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
import java.util.LinkedList
import scala.collection.JavaConversions._
import scampi.invariants._

/**
 * This is the main engine of the simulation.
 * Every Process in the simulation should wait, require resource ... on an instance of this class.
 * @author pschaus
 */
class Model extends EventModel{
   
	val clock = new PQCounter(this, 0)
		
	private val processes = new LinkedList[Process]()
	
	def addProcess(p : Process) {
	  processes.addLast(p)
	}
	
	def simulate(horizon: Int,verbose: Boolean = true) {
		val it = processes.iterator 
		while(it.hasNext) { 
			it.next().simulate()
		}

		while (clock.nonEmpty && clock() <= horizon) {
			val e = clock.next
						
			if(verbose && e.time <= horizon ){
				println("-----------> time: "+ clock())
			}
			if(clock() <= horizon){
			  withDelay{
				e.process()
			  }				
			}
		}
	}
	def print(s: String){
	  println(clock() + ": " + s)
	}
	def time(o: Any): Double = {
	  clock()
	}
//	def frequency[_](state: State[_]) = new Frequency(this,state)
	
//	def waitt(duration : Double)(block : => Unit):Unit =  {
//		assert(duration >= 0)
//		addEvent(new WaitEvent(clock + duration, block))
//	}
//	
//    def waitt(duration : Int)(block : => Unit) {
//		waitt(duration.toDouble)(block)
//	}
//    
    def wait(duration : Double):Unit@suspendable= {
		waitFor( clock === clock() + duration.toDouble)
		()
		
    }
def wait(duration : Int):Unit@suspendable={wait(duration.toDouble)}

//  def waitFor[A](ev: Signal[A], f: A => Boolean): Unit @suspendable = {
//    if ( !f(ev())){ 
//    var obs: Reaction[A] = null
//    shift { k: (Unit => Unit) =>
//      obs = when(ev) { (x: A) =>
//        if (f(x)) {
//          k()
//        }
//        true
//      }
//    }
//    obs.dispose()
//    }
//  }

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