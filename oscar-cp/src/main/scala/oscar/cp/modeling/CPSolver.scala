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
import scala.util.continuations._
import scala.collection.mutable.Stack
import oscar.algo.reversible._
import oscar.util._
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.Pareto
import oscar.cp.constraints.ParetoConstraint

class CPSolver() extends CPStore() {

  def +=(cons: Constraint, propagStrength: CPPropagStrength = CPPropagStrength.Weak): Unit = {
    this.add(cons, propagStrength)
  }
  
  var objective = new CPObjective(this, Array[CPObjectiveUnit]());
  
  private val decVariables = scala.collection.mutable.Set[CPIntVar]()
  var lastSol = new CPSol(Set[CPIntVar]())
  var paretoSet: Pareto[CPSol] = new ListPareto[CPSol](Array())
  var recordNonDominatedSolutions = false
  
  def nonDominatedSolutions: Seq[CPSol] = paretoSet.toList
  def nonDominatedSolutionsObjs: Seq[IndexedSeq[Int]] = paretoSet.objectiveSols 
  
  def addDecisionVariables(x: Iterable[CPIntVar]): Unit = x.foreach(decVariables += _)
  
  def addDecisionVariables(x: CPIntVar*): Unit = x.foreach(decVariables += _)
  
  private def recordSol(): Unit = {
    lastSol = new CPSol(decVariables.toSet)
  }
  
  def obj(objVar: CPIntVar): CPObjectiveUnit = objective(objVar)

  def optimize(obj: CPObjective): CPSolver = {
    objective = obj
    postCut(obj)
    this
  }

  def minimize(objective: CPIntVar): CPSolver = minimize(Seq(objective): _*)
  
  def minimize(objectives: CPIntVar*): CPSolver = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMinimize(_)): _*))

  def maximize(objective: CPIntVar): CPSolver = maximize(Seq(objective): _*)

  def maximize(objectives: CPIntVar*): CPSolver = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMaximize(_)): _*))
  
  def paretoMinimize(objective: CPIntVar): CPSolver = paretoOptimize((objective, false))  
  
  def paretoMinimize(objectives: CPIntVar*): CPSolver = paretoOptimize(objectives.map((_, false)): _*)
  
  def paretoMaximize(objective: CPIntVar): CPSolver = paretoOptimize((objective, true))
  
  def paretoMaximize(objectives: CPIntVar*): CPSolver = paretoOptimize(objectives.map((_, true)): _*)
  
  def paretoOptimize(objVarModes: (CPIntVar, Boolean)): CPSolver = paretoOptimize(Seq(objVarModes): _*)
  
  def paretoOptimize(objVarMode: (CPIntVar, Boolean)*): CPSolver = {
    
    // objVar of each objective
    val objectives = objVarMode.map(_._1).toArray
    // true if objective i has to be maximized
    val isMax = objVarMode.map(_._2).toArray

    recordNonDominatedSolutions = true
    objective = new CPObjective(this, (for (i <- 0 until isMax.size) yield {
      if (isMax(i)) new CPObjectiveUnitMaximize(objectives(i))
      else new CPObjectiveUnitMinimize(objectives(i))
    }): _*)
    postCut(objective)
    objective.objs.foreach(_.tightenMode = TightenType.NoTighten)
    
    addDecisionVariables(objectives)
    paretoSet = new ListPareto[CPSol](isMax)
    // Adds a dominance constraint with all objectives in minimization mode
    addCut(new ParetoConstraint(paretoSet, isMax, objectives.toArray))
    this
  }
  
  @deprecated("solve is the default behavior of CPSolver and does not need to be specified anymore.", "1.0")
  def solve(): CPSolver = this

  @deprecated("constraints do not need to be stated in the subectTo block anymore.", "1.0")
  def subjectTo(constraintsBlock: => Unit): CPSolver = {
    try {
      constraintsBlock
    } catch {
      case ex: NoSolutionException => println("No Solution, inconsistent model")
    }
    this
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
}
