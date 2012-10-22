/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cp.modeling

import oscar.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.constraints._
import scala.util.continuations._
import scala.collection.mutable.Stack
import oscar.cp.scheduling.CumulativeActivity
import oscar.reversible._

class NoSol(msg : String) extends Exception(msg)

class CPSolver() extends Store() {
    
    case class LNS(val nbRestarts: Int, val nbFailures: Int, val restart: () => Unit ) 
    var lns: Option[LNS] = None
    
    private var lastLNSRestartCompleted = false
  
    /**
     * @return true if the last lns restart was caused because of completed exploration of search tree, 
     * false otherwise (i.e. limit on the number failure reached)
     */
    def isLastLNSRestartCompleted = lastLNSRestartCompleted
    
    def lns(nbRestarts: Int, nbFailures: Int)(restart: => Unit) {
	  lns = Option(new LNS(nbRestarts,nbFailures,() => restart))
	}

	/**
	 * @param block a code block
	 * @return the time (ms) to execute the block
	 */
	def getTime(block : => Unit) : Long = {
		val t0 = System.currentTimeMillis()
		block
		System.currentTimeMillis - t0
	}

	def +=(cons : Constraint, propagStrength : CPPropagStrength = CPPropagStrength.Weak) : Unit = {
		this.add(cons, propagStrength)
	}
	
	var stateObjective: Unit => Unit = Unit => Unit

	def minimize(obj : CPVarInt) : CPSolver = {
		stateObjective = Unit => {
		  val o = new CPObjectiveMinimize(obj)
		  objective = o
		  post(o)
		}
		solveAll()
		this
	}
	
	
	//(obj1,weight1,id1,"name1"),(obj2,weight2,id2,"name2")
	
	def minimize(objectives: CPVarInt*): CPSolver = {
	  stateObjective = Unit => {
		  val o = new CPObjectiveMinimize(objectives:_*)
		  objective = o
		  post(o)
	  }
	  solveAll()
	  this
	}
	
	

	def maximize(obj : CPVarInt) : CPSolver = {
		stateObjective = Unit => {
		  val o = new CPObjectiveMaximize(obj)
		  objective = o
		  post(o)

		}
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
	def allBounds(vars : IndexedSeq[CPVarInt]) = vars.map(_.isBound).foldLeft(true)((a, b) => a & b)



	def minDom(x : CPVarInt) : Int = x.size
	def minRegre(x : CPVarInt) : Int = x.max - x.min
	def minDomMaxDegree(x : CPVarInt) : (Int, Int) = (x.size, -x.constraintDegree)
	def minVar(x : CPVarInt) : Int = 1
	def maxDegree(x : CPVarInt) : Int = -x.constraintDegree

	def minVal(x : CPVarInt) : Int = x.min
	def maxVal(x : CPVarInt) : Int = x.max
	def minValminVal(x : CPVarInt) : (Int, Int) = (x.min, x.min)

	/**
	 * Binary First Fail on the decision variables vars
	 */
	def binaryFirstFail(vars : Array[CPVarInt], valHeuris : (CPVarInt => Int) = minVal) : Unit @suspendable = {
	    while (!allBounds(vars)) {
			val unbound = vars.filter(!_.isBound)
			val minDomSize = unbound.map(_.size).min
			val x = unbound.filter(_.getSize == minDomSize).first
			val v = valHeuris(x)
			branch(post(x == v))(post(x != v)) // right alternative			
	    }
	}

	/**
	 * Binary search on the decision variables vars with custom variable/value heuristic
	 */
	def binary(vars : Array[CPVarInt], varHeuris : (CPVarInt => Int) = minVar, valHeuris : (CPVarInt => Int) = minVal) : Unit @suspendable = {
		while (!allBounds(vars)) {
			val unbound = vars.filter(!_.isBound)
			val heuris = unbound.map(varHeuris(_)).min
			val x = unbound.filter(varHeuris(_) == heuris).first
			val v = valHeuris(x)
			branch(post(x == v))(post(x != v)) // right alternative
		}
	}

	/**
	 *
	 */
	def binaryFirstFail(vars : CPVarInt*) : Unit @suspendable = {
		binary(vars.toArray, valHeuris = minVal)
	}

	/**
	 * Binary search on the decision variables vars, selecting first the variables having the max number
	 * of propagation methods attached to it.
	 */
	def binaryMaxDegree(vars : Array[CPVarInt]) : Unit @suspendable = {
		binary(vars, varHeuris = maxDegree, valHeuris = minVal)
	}
	
	/**
	 * Binary search on the decision variables vars, splitting the domain of the selected variable on the 
	 * median of the values (left : <= median, right : > median)
	 */
	def binaryDomainSplit(vars : Array[CPVarInt], varHeuris : (CPVarInt => Int) = minVar, valHeuris : (Int => Int) = i => i) : Unit @suspendable = {
		
		while (!allBounds(vars)) {
			
			val unbound = vars.filter(!_.isBound)
			val heuris  = unbound.map(varHeuris(_)).min
			val x       = unbound.filter(varHeuris(_) == heuris).first	
			
			val vals     = x.toArray.sortBy(valHeuris)
			val median   = vals(vals.size/2)
			
			branch(post(x <= median))(post(x > median)) 
		}
	}

	def printStats() {
		println("time(ms)", time)
		println("#bkts", sc.nFail)
		println("time in fix point(ms)", getTimeInFixPoint())
		println("time in trail restore(ms)", getTrail().getTimeInRestore())
		println("max trail size", getTrail().getMaxSize())
	}
	
	
	override def exploration(block: => Unit @suspendable ): Unit  =  {
	  val t1 = System.currentTimeMillis()
	  stateObjective()
	  var nbRestart = 0
	  var maxRestart = 1
	  
	  val relax = lns match {
		   case None => () => Unit
		   case Some(LNS(nbRestart,nbFailures,restar)) => {
		     maxRestart = nbRestart
		     failLimit = nbFailures
		     restar
		   }
	  }  

	  reset {
        shift { k1: (Unit => Unit ) =>
          val b = () => {
        	  	sc.start()
        	  	propagate()
        	  	if (!isFailed()) {
        	  		block
        	  	} else {
        	  	  shift { k: (Unit => Unit) => k() }
        	  	}
                if (!isFailed()) {
                	if (solveOne) {
                	  sc.reset()
                	  k1() // exit the exploration block
                	}
                }
          }

          def restart(relaxation: Boolean = false) {
             popAll()
             pushState()
             if (relaxation) {
               sc.reset()
               relax()
               
             }
             if (!isFailed()) {
                 sc.reset()
                 nbRestart += 1 
                 reset {
                   b()  	  
                   if (!isFailed()) objective.tighten()
      	         }
                 if (!sc.exit) sc.explore() // let's go, unless the user decided to stop
             }
          }
          restart(false) // first restart, find a feasible solution so no limit
          for (r <- 2 to maxRestart; if (!objective.isOptimum() && !sc.exit)) {
             restart(true)
             if (sc.isLimitReached) {
               lastLNSRestartCompleted = false
               if (!silent) print("!") 
             }
             else {
               lastLNSRestartCompleted = true
               if (!silent) print("R") 
             }
          }
          k1() // exit the exploration block       
        } 
      }
	  time = System.currentTimeMillis() - t1
    }	
	
}

object CPSolver {

	/**
	 * Creates a new CP Solver
	 */
	def apply() : CPSolver = {
		new CPSolver()
	}
}
