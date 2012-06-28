/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.modeling

import oscar.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.constraints._
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
	
	/**
	 * Set the maximum number of fails for the search
	 */
	def failLimit(nbFailMax: Int) {
	  sc.failLimit = nbFailMax 
	}
	
	def minVal(x: CPVarInt): Int = x.getMin()
	def maxVal(x: CPVarInt): Int = x.getMax()
	
	/**
     * Binary First Fail on the decision variables vars
     */
    def binaryFirstFail(vars: Array[CPVarInt], valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
     while (!allBounds(vars)) {
    	   val unbound = vars.filter(!_.isBound)
    	   val minDomSize = unbound.map(_.getSize()).min 
    	   val x = unbound.filter(_.getSize == minDomSize).first
           val v = valHeuris(x)
    	   branch (post(x == v))(post(x != v))// right alternative
     }
    }
	
	def binaryFirstFail(vars: CPVarInt*): Unit @suspendable = {
     binaryFirstFail(vars.toArray,valHeuris = minVal)
    }

	/**
     * Binary search on the decision variables vars
     */
    def binary(vars: Array[CPVarInt]): Unit @suspendable = {
     while (!allBounds(vars)) {
    	   val x = vars.filter(!_.isBound).first
           val v = x.getMin()
    	   branch (post(x == v))(post(x != v))// right alternative
     }
    }

	/**
     * Binary search on the decision variables vars, selecting first the variables having the max number
     * of propagation methods attached to it.
     */
    def binaryMaxDegree(vars: Array[CPVarInt]): Unit @suspendable = {
     while (!allBounds(vars)) {
    	   val unbound = vars.filter(!_.isBound)
    	   val maxDegree = unbound.map(_.getConstraintDegree()).max 
    	   val x = unbound.filter(_.getConstraintDegree() == maxDegree).first
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

