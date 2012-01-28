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

/**
 * Every simulated object taking part in the simulation should extend this class.
 * @author Pierre Schaus, Sebastien Mouthuy
 */
class Process (m: Model, name : String = "Process"){

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
}