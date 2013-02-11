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
import scala.util.continuations._

/**
 * Every simulated object taking part in the simulation should extend this class.
 * @author Pierre Schaus, Sebastien Mouthuy
 */
abstract class Process (m: Model, name : String = "Process"){

	m.addProcess(this)
	private var suspending = false
	private var suspended = {}
	
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