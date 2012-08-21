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

package oscar.reversible;

import java.util.Random
import java.util.Stack


import scala.util.continuations._
import oscar.search.DummyObjective;
import oscar.search.Objective;
import oscar.search._



/**
 * Class representing a reversible search node, that is a node able to restore all
 * the reversible state attached to it (see Reversibles). <br>
 * A reversible search node is used to find solution by exploration of search tree (see Search).
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleSearchNode {
	
	var magic = 0	
	var trail = new Trail();
	val pointerStack = new Stack[TrailEntry]()
	var objective: Objective = new DummyObjective() 	
	val random: Random = new Random(0)
	val failed = new ReversibleBool(this,false)
	var stateObjective : Unit => Unit = Unit => Unit
	var sc: SearchController = new DFSSearchController(this)
    case class LNS(val nbRestarts: Int, val nbFailures: Int, val restart: () => Unit ) 
    var lns: Option[LNS] = None
    protected var lastLNSCompleted : Boolean = false
    var time : Long = 0
    var solveOne = false
    
    def lns(nbRestarts: Int, nbFailures: Int)(restart: => Unit) {
		
	  lns = Option(new LNS(nbRestarts,nbFailures,() => restart))
	}
	
	/**
	 * 
	 * @return True if the last LNS restart has explored all the possible solutions
	 */
	def islastLNSCompleted = lastLNSCompleted
	
	/**
	 * 
	 * @return The number of failures allowed during a LNS restart
	 */
	def lnsFailuresLimit = sc.failLimit
	
	/**
	 * 
	 * @param The number of failures allowed during a LNS restart (not modified if negative integer)
	 */
	def lnsFailuresLimit_= (x : Int) { if (x >= 0) sc.failLimit = x }
	
	/**
	 * 
	 * @return The Random generator of this node potentially used in other algorithms
	 */
	def getRandom() = random

    /**
     *
     * @return  true if this node can surely not lead to any solution
     */
	def isFailed(): Boolean = failed.value
	

	def fail() {
		failed.setValue(true)
	}
	
	/**
	 * Exit the search in progress and/or the LNS if any
	 */
	def stop() {
	  sc.stop()
	}

    /**
     *
     * @return an objective if any, null otherwise
     */
	def getObjective() = objective

    /**
     * Set the objective to optimize
     * @param obj
     */
	def setObjective(obj: Objective) {
		this.objective = obj;
	}
	
	def getMagic() = magic
	
	def getTrail() = trail

    /**
     * Store the current state of the node on a stack.
     */
	def pushState() {
		magic += 1
		pointerStack.push(trail.getTopEntry())
	}

    /**
     * Restore state on top of the stack of states and remove it from the stack.
     */
	def pop() {
		trail.restoreUntil(pointerStack.pop())
		magic += 1 // increment the magic because we want to trail again
	}

    /**
     * Restore the node to its initial state
     */
	def popAll() {
		while (!pointerStack.empty()) {
			trail.restoreUntil(pointerStack.pop())
		}
		magic +=1 // increment the magic because we want to trail again
	}
	
	override def toString() = {
		"ReversibleSearchNode: nbPushed"+pointerStack.size()+" currentTrailSize:"+trail.getSize();
	}
	
	
	
	def branch(left: => Unit)(right: => Unit) = {
      shift { k: (Unit => Unit) =>
        if (!isFailed) {
        sc.addChoice(new MyContinuation("right", {
          if (!isFailed) right
          if (!isFailed()) k()}))
        }
        if (!isFailed()) left
        if (!isFailed()) k()
      }
    }
	
	def branchAll[A](indexes: Seq[A])(f: A => Unit) = {
	  
      shift { k: (Unit => Unit) =>
        val first = indexes.first
        for (i <- indexes.reverse; if (i != first)) {
           sc.addChoice(new MyContinuation("i", {
        	   f(i)
          	   if (!isFailed()) k()}))
            
        }
        f(first)
        if (!isFailed()) k()
      }
    }	
	
	

	
	def branchOne(left: => Unit): Unit @suspendable = {
      shift { k: (Unit => Unit) =>
        left
        if (!isFailed()) k()
      }
    }
	
    
	def exploration(block: => Unit @suspendable ): Unit  =  {
	  val t1 = System.currentTimeMillis()
	  stateObjective()
	  var nbRestart = 0
	  var maxRestart = 1
	  var limit = sc.failLimit
	  
	  val relax = lns match {
		   case None => () => Unit
		   case Some(LNS(nbRestart,nbFailures,restart)) => {
		     maxRestart = nbRestart
		     limit = nbFailures
		     restart
		   }
	  }  


	  reset {
        shift { k1: (Unit => Unit ) =>
          val b = () => {
        	  	sc.start()
                block
                if (!isFailed()) {
                	sc.failLimit = limit
                	if (solveOne) {
                	  val nbFail = sc.nbFail
                	  sc.reset()
                	  sc.nbFail = nbFail
                	  k1() // exit the exploration block
                	}
                }
          }

          def restart(relaxation: Boolean = false) {
             popAll()
             pushState()
             if (relaxation) {
            	 relax()
             }
               if (!isFailed()) {
                 sc.reset()
                 nbRestart += 1 
                 reset {
                   b()  	  
                   if (!isFailed()) getObjective().tighten()
      	         }
                 if (!sc.exit) sc.explore() // let's go, unless the user decided to stop
               }
          }
          sc.failLimit = limit
          restart(false) // first restart, find a feasible solution so no limit
          for (r <- 2 to maxRestart; if (!getObjective().isOptimum() && !sc.exit)) {
             restart(true)
             if (sc.limitReached) {
            	 println("failLimit " + sc.failLimit)
            	 lastLNSCompleted = false
            	 print("!")
             } else {
            	 println("failLimit " + sc.failLimit)
            	 lastLNSCompleted = true
            	 print("R")
             }
          }
          k1() // exit the exploration block       
        } 
      }
	  time = System.currentTimeMillis() - t1
    }	
}
