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
  
    case class LNS(val nbRestarts: Int, val nbFailures: Int, val restart: () => Unit ) 
	
	val branchings = new BranchingCombinator()
	
	val search = new Explorer(this, branchings)
	
	var startSearch : Unit => Unit = Unit => search.solveOne()
	
	var stateObjective : Unit => Unit = Unit => Unit
	
	var time : Long = 0
	
	var tree = false
	
	var restart: Option[Restart] = None
	
	var lns: Option[LNS] = None
	
	class Closure(msg: String, block: => Unit)  {
      def run() = {
        block
      }
      override def toString = msg
    }
	
	val sc = new SearchController(this)
	
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
		startSearch = Unit => search.solveOne() 
		this
	}
	
	def solveAll() : CPSolver = {
		startSearch = Unit => {
		 lns match {
		   case None => search.solveAll()
		   case Some(LNS(nbRestart,nbFailures,rest)) => {
		     // creates the restart object
		     val r = new Restart() {
		        override def restart() {
				  rest()
				}
		     }
		     search.solveLNS(nbRestart,nbFailures,r) 
		   }
		 } 
		} 
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
	
	def addBranching(searchBlock : Branching) {
		branchings.addBranching(searchBlock)
	}
	
	def addBranching(searchBlock : => Array[Alternative] ) {
		branchings.addBranching(
						new Branching() {
							override def getAlternatives(): Array[Alternative] = {
									searchBlock
							}
						})
	}
	
	private def run() {
	  	stateObjective()
		startSearch()
	}
	
	def exploring(searchBlock : => Array[Alternative]) {
		if (this.getStatus() != CPOutcome.Failure) {
			time = getTime {
				branchings.addBranching(
						new Branching() {
							override def getAlternatives(): Array[Alternative] = {
									searchBlock
							}
						})
						time = getTime { run() }
			}
		}
	}
	
	def exploring(branching : Branching*) : CPSolver = {
		if (this.getStatus() != CPOutcome.Failure) {
			branching.foreach(branchings.addBranching(_))
			time = getTime {run()}
		} else {
          println("failed model")
        }
		this
	}
	
	def exploring(searchBlock : => Unit) : CPSolver = {
		searchBlock //execute the search block adding the branchings
		if (this.getStatus() != CPOutcome.Failure) {
			time = getTime {run()}
		}
		this
	}
	
	def lns(nbRestarts: Int, nbFailues: Int)(restart: => Unit) {
	  lns = Option(new LNS(nbRestarts,nbFailues,() => restart))
	}

	def onSolution(block: => Unit ) : CPSolver = {
		search.onSolution(block)
		this
	}
	
	def printStats() {
		println("time(ms)",time)
		println("#bkts",search.getNbBkts())
		println( "time in fix point(ms)",getTimeInFixPoint())
		println( "time in trail restore(ms)",getTrail().getTimeInRestore())
		println( "max trail size",getTrail().getMaxSize())
	}
	
	def branchOn(vals : Range,filter : (Int => Boolean), by : (Int => Double) ) (cons: (Int => Constraint)) : Array[Alternative] = {
		val alts = for(v <- vals; if filter(v))  yield (by(v),new CPAlternative(this,cons(v)))
		if (alts.isEmpty) fail()
		val sortedalts = alts.toList.sort((e1,e2) => (e1._1 < e2._1))
		sortedalts.map(_._2).toArray
	}
	
	def branchOn(x : CPVarInt, by : (Int => Double) = (v => v) ) (cons: (Int => Constraint)) : Array[Alternative] = {
		branchOn(x.getMin to x.getMax,x.hasValue(_),by)(cons)
	}
	
	
	def branchOn(c : Constraint*) : Array[Alternative] = {
		c.map(cons => new CPAlternative(this,cons)).toArray
	}	
    
    def branch(left: => Unit)(right: => Unit): Unit @suspendable = {
      shift { k: (Unit => Unit) =>
        sc.addChoice(new MyContinuation("right", {
          right
          k()}))
        left
        k()
      }
    }
    
	def exploration(block: => Unit @suspendable ): Unit  =  {
	  stateObjective()
      val b = () => block
	  reset {
        shift { k1: (Unit => Unit ) =>
          sc.start(new MyContinuation("exit", {k1()}))
          reset {
            b()  	  
            if (!isFailed()) getObjective().tighten()
            sc.fail()
      	   }  
        } 
      }
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

