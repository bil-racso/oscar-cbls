/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.modeling

import scampi.cp._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.constraints._
import scampi.search._
import java.io._

class Explorer(val cp: Store, val br: Branching) extends Search(cp,br) {

	def this(cp:Store,block: () => Array[Alternative]) {
		this(cp, new Branching() {
			override def getAlternatives(): Array[Alternative] = {
				block()
			}
		})
	}
	
	/**
	 * add a call back block executed when a solution is found i.e.
	 * new best solution in B&B or new feasible solution when searching for all the solutions)
	 * @param block
	 */
	def onSolution(block: => Unit ) {
		addSolutionObserver(new SolutionObserver() {
    	  override def solutionFound() {
    	 	  block
    	  }
		})   
	}
	
	/**
	 * Large Neighborhood Search (LNS). Apply te relaxation block after nbFail (if a feasible sol has already been found)
	 * @param nbFail
	 * @param block is a set of instructions defining the relaxation
	 */
//	def relaxAfter(nbFail : Int)(block: => Unit ) {
//		lnsOnFailure(nbFail, new Restart() {
//			override def restart() {
//				block
//			}
//		})
//	}
	
	
	

	
	/**
	 * limit the execution time of the search
	 * @param seconds, a positive integer
	 */
	def timeOut(seconds:Int) = {
		setTimeLimit(seconds)
	}
	
}

