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

package oscar.cp.modeling

import oscar.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.search._
import oscar.cp.constraints._
import scala.util.continuations._
import scala.collection.mutable.Stack
import oscar.cp.scheduling.CumulativeActivity
import oscar.reversible._

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
	def allBounds(vars: IndexedSeq[CPVarInt]) = vars.map(_.isBound).foldLeft(true)((a,b) => a & b)
	
	/**
	 * Set the maximum number of fails for the search
	 */
	def failLimit(nbFailMax: Int) {
	  sc.failLimit = nbFailMax 
	}
	
	def minDom(x: CPVarInt): Int = x.size
	def minRegre(x: CPVarInt): Int = x.max-x.min
	def minDomMaxDegree(x: CPVarInt): (Int,Int) = (x.size,-x.constraintDegree)
	def minVar(x: CPVarInt): Int = 1
	def maxDegree(x: CPVarInt): Int = -x.constraintDegree
	
	
	def minVal(x: CPVarInt): Int = x.min
	def maxVal(x: CPVarInt): Int = x.max
	def minValminVal(x: CPVarInt): (Int,Int) = (x.min,x.min)
	
	/**
     * Binary First Fail on the decision variables vars
     */
    def binaryFirstFail(vars: Array[CPVarInt], valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
     while (!allBounds(vars)) {
    	   val unbound = vars.filter(!_.isBound)
    	   val minDomSize = unbound.map(_.size).min 
    	   val x = unbound.filter(_.getSize == minDomSize).first
           val v = valHeuris(x)
    	   branch (post(x == v))(post(x != v))// right alternative
     }
    }
	
	/**
     * Binary search on the decision variables vars with custom variable/value heuristic
     */
	def binary(vars: Array[CPVarInt], varHeuris: (CPVarInt => Int) = minVar, valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
     while (!allBounds(vars)) {
    	   val unbound = vars.filter(!_.isBound)
    	   val heuris = unbound.map(varHeuris(_)).min 
    	   val x = unbound.filter(varHeuris(_) == heuris).first
           val v = valHeuris(x)
    	   branch (post(x == v))(post(x != v))// right alternative
     }
    }
		
	
	/**
	 * 
	 */
	def binaryFirstFail(vars: CPVarInt*): Unit @suspendable = {
      binary(vars.toArray,valHeuris = minVal)
    }


	/**
     * Binary search on the decision variables vars, selecting first the variables having the max number
     * of propagation methods attached to it.
     */
    def binaryMaxDegree(vars: Array[CPVarInt]): Unit @suspendable = {
      binary(vars, varHeuris = maxDegree, valHeuris = minVal)
    } 
    
    
	def setTimes(activities : Array[CumulativeActivity]) : Unit @suspendable = {
		val n = activities.size
		val Activities = 0 until n
		// Non fixed activities
		val selectable = Array.fill(n)(new ReversibleBool(this,true))
		val oldEST = Array.fill(n)(new ReversibleInt(this,-1))
		def updateSelectable() =  (Activities).filter(i => oldEST(i).value < activities(i).est).foreach(selectable(_).value = true)
		def selectableIndices() = (Activities).filter(i => selectable(i).value && !activities(i).start.isBound)
		def allStartBounds() = activities.forall(i => i.start.isBound)
		def updateAndCheck() = {
		  updateSelectable()
		  if (selectableIndices().isEmpty && !allStartBounds()) this.fail()
		}
		while (!allStartBounds()) {
			updateSelectable()
			val (est,ect) = selectableIndices().map(i => (activities(i).est,activities(i).ect)).min
			// Select the activity with the smallest EST, ECT as tie breaker
			val x = selectableIndices().filter(i => activities(i).est == est && activities(i).ect == ect).first
			branch {  
				this.post(activities(x).start == est)
				if (!this.isFailed) updateAndCheck()
			} {
				selectable(x).value = false
				oldEST(x).value = est
				updateAndCheck()
			}	
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

