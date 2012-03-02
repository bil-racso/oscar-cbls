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

import scampi.search._
import scampi.cp._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.constraints._
import scala.util.continuations._
import scala.collection.mutable.Stack

class NoSol(msg : String) extends Exception(msg)

class CPSolver() extends Store() { 

	/**
	 * @param block a code block
	 * @return the time (ms) to execute the block
	 */
	def getTime(block: => Unit): Long = {
			val t0 = System.currentTimeMillis()
			block
			System.currentTimeMillis - t0
	}
	
	def += (cons : Constraint,propagStrength: CPPropagStrength = CPPropagStrength.Weak ) : Unit = {
		 this.add(cons, propagStrength)
	}
	
	def minimize(obj : CPVarInt) : CPSolver = {
	    stateObjective = Unit => minimization(obj)
		solveAll() 
		this
	}
	
	def maximize(obj : CPVarInt) : CPSolver = {
		stateObjective = Unit => maximization(obj)
		solveAll()
		this
	}
		
	def solve() : CPSolver = {
	    solveOne = true
		this
	}
	
	def solveAll() : CPSolver = {
	    solveOne = false
		this
	}
	
	def subjectTo(constraintsBlock : => Unit) : CPSolver = {
		try {
			constraintsBlock
		} catch {
			case ex : NoSol => println("No Solution, inconsistent model")
		}
		this
	}
	
	/**
	 * return true if every variable is bound
	 */
	def allBounds(vars: IndexedSeq[CPVarInt]) = vars.map(_.isBound()).foldLeft(true)((a,b) => a & b)
	
	
	def binaryFirstFail(vars: CPVarInt*): Unit @suspendable = {
	  binaryFirstFail(vars.toIndexedSeq)
	}
	
	def binaryFirstFail(vars: Array[CPVarInt]): Unit @suspendable = {
	  binaryFirstFail(vars.toIndexedSeq)
	}
	
	/**
	 * Set the maximum number of fails for the search
	 */
	def failLimit(nbFailMax: Int) {
	  sc.failLimit = nbFailMax 
	}
	
	/**
     * Binary First Fail on the decision variables vars
     */
    def binaryFirstFail(vars: IndexedSeq[CPVarInt]): Unit @suspendable = {
     while (!allBounds(vars)) {
    	   val unbound = vars.filter(!_.isBound)
    	   val minDomSize = unbound.map(_.getSize()).min 
    	   val x = unbound.filter(_.getSize == minDomSize).first
           val v = x.getMin()
    	   branch (post(x == v))(post(x != v))// right alternative
     }
    }
	
	def printStats() {
		println("time(ms)",time)
		println("#bkts",sc.nbFail)
		println("time in fix point(ms)",getTimeInFixPoint())
		println("time in trail restore(ms)",getTrail().getTimeInRestore())
		println("max trail size",getTrail().getMaxSize())
	}
	
    
	
}

object CPSolver {

  /**
   * Creates a new CP Solver
   */
  def apply(): CPSolver = {
    new CPSolver()
  }
}

