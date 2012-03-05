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
import scala.util.continuations._
import scampi.invariants._

/**
 * Every simulated object taking part in the simulation should extend this class.
 * @author Pierre Schaus, Sebastien Mouthuy
 */
abstract class Process (m: Model, name : String = "Process"){

	m.addProcess(this)
	private var suspending = false
	private var suspended = {}
	
	def is[A](s: State[A]) = s.isIn.filter(_ == true) 
	  
	def suspend(): Unit @ suspendable = {
//		if (suspending) {
//			//throw new RuntimeException("The process " + name + " is already suspending");
//		}
		suspending = true
		shift{k:(Unit=>Unit)=>
		  suspended = {k()}
		}
	}
	
	def resume(){
//		if (!suspending){
//			//throw new RuntimeException("The process " + name + " is not suspending");
//		}
		suspending = false
		suspended
	}
	
	/**
	 * Entry point of the simulation for this process
	 */
	def start(): Unit @ suspendable
	
	/**
	 * Properly start the simulation of this process (method normally called by the engine, not the modeler).
	 */
	def simulate(){
	  reset {
	    start()
	  }
	}
	
	
}