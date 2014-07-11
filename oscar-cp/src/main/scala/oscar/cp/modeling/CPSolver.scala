/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.cp.modeling

import oscar.algo.search._
import oscar.cp._
import oscar.cp.core._
import oscar.cp.constraints._
import scala.collection.mutable.Stack
import oscar.algo.reversible._
import oscar.util._
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.Pareto
import oscar.cp.constraints.ParetoConstraint

class CPSolver(propagStrength: CPPropagStrength) extends CPOptimizer(propagStrength) {
  
  def this() = this(CPPropagStrength.Weak)
  
  private val decVariables = scala.collection.mutable.Set[CPIntVar]()
  
  var lastSol = new CPSol(Set[CPIntVar]())
  
  def addDecisionVariables(x: Iterable[CPIntVar]): Unit = x.foreach(decVariables += _)
  
  def addDecisionVariables(x: CPIntVar*): Unit = x.foreach(decVariables += _)
  
  private def recordSol(): Unit = {
    lastSol = new CPSol(decVariables.toSet)
  }
  
  override def beforeStartAction(): Unit = { deactivateNoSolExceptions() }

  /**
   * return true if every variable is bound
   */
  def allBounds(vars: IndexedSeq[CPIntVar]): Boolean = {
	var i = 0 
	val s = vars.size
	while (i < s) {
	  if (!vars(i).isBound) return false
	  i += 1
	}
    true
  }
  
  override def minimize(objective: CPIntVar): CPSolver = {
    super.minimize(Seq(objective): _*)
    this
  }

  override def maximize(objective: CPIntVar): CPSolver = {
    super.maximize(Seq(objective): _*)
    this
  }
  
  override def update(): Unit = propagate()
  
  override def solFound(): Unit = {
    super.solFound()
    lastSol = new CPSol(decVariables.toSet)
    if (recordNonDominatedSolutions) {
      if (!silent) println("new solution:"+objective.objs.map(_.objVar.value).toArray.mkString(","))
      paretoSet.insert(lastSol, objective.objs.map(_.objVar.value):_*)
    }
    objective.tighten()
  }
}

object CPSolver {
  
  /** Creates a new CP Solver */
  def apply(): CPSolver = new CPSolver()
  
  /** Creates a new CP Solver with `propagStrength` as default level of propagation */
  def apply(propagStrength: CPPropagStrength) = new CPSolver(propagStrength)
  
  /** Creates a new CP Solver with `Weak` as default level of propagation */
  def weak: CPSolver = new CPSolver(CPPropagStrength.Weak)
  
  /** Creates a new CP Solver with `Medium` as default level of propagation */
  def medium: CPSolver = new CPSolver(CPPropagStrength.Medium)
  
  /** Creates a new CP Solver with `Strong` as default level of propagation */
  def strong: CPSolver = new CPSolver(CPPropagStrength.Strong)
}
