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
import oscar.util._
import oscar.cp.multiobjective.ListPareto
import oscar.cp.multiobjective.Pareto
import oscar.cp.constraints.ParetoConstraint


class CPSolver() extends CPStore() {

  def +=(cons: Constraint, propagStrength: CPPropagStrength = CPPropagStrength.Weak): Unit = {
    this.add(cons, propagStrength)
  }
  
  var objective = new CPObjective(this, Array[CPObjectiveUnit]());
  
  private var stateObjective: Unit => Unit = Unit => Unit
  
  private val decVariables = scala.collection.mutable.Set[CPVarInt]()
  var lastSol = new CPSol(Set[CPVarInt]())
  var paretoSet: Pareto[CPSol] = new ListPareto[CPSol](Array())
  var recordNonDominatedSolutions = false
  
  def nonDominatedSolutions: Seq[CPSol] = paretoSet.toList
  def nonDominatedSolutionsObjs: Seq[IndexedSeq[Int]] = paretoSet.objectiveSols 
  
  def addDecisionVariables(x: Iterable[CPVarInt]) {
    x.foreach(decVariables += _)
  }
  
  def addDecisionVariables(x: CPVarInt*) {
    x.foreach(decVariables += _)
  }
  
  private def recordSol() {
    lastSol = new CPSol(decVariables.toSet)
  }
  
  def obj(objVar: CPVarInt): CPObjectiveUnit = {
    objective(objVar)
  }

  def optimize(obj: CPObjective): CPSolver = {
    stateObjective = Unit => {
      objective = obj
      postCut(obj)
    }
    this
  }

  def minimize(objective: CPVarInt): CPSolver = minimize(Seq(objective): _*)
  
  def minimize(objectives: CPVarInt*): CPSolver = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMinimize(_)): _*))

  def maximize(objective: CPVarInt): CPSolver = maximize(Seq(objective): _*)

  def maximize(objectives: CPVarInt*): CPSolver = 
    optimize(new CPObjective(this, objectives.map(new CPObjectiveUnitMaximize(_)): _*))
  
  def paretoMinimize(objective: CPVarInt): CPSolver = paretoOptimize((objective, false))  
  
  def paretoMinimize(objectives: CPVarInt*): CPSolver = paretoOptimize(objectives.map((_, false)): _*)
  
  def paretoMaximize(objective: CPVarInt): CPSolver = paretoOptimize((objective, true))
  
  def paretoMaximize(objectives: CPVarInt*): CPSolver = paretoOptimize(objectives.map((_, true)): _*)
  
  def paretoOptimize(objVarModes: (CPVarInt, Boolean)): CPSolver = paretoOptimize(Seq(objVarModes): _*)
  
  def paretoOptimize(objVarMode: (CPVarInt, Boolean)*): CPSolver = {
    
    // objVar of each objective
    val objectives = objVarMode.map(_._1).toArray
    // true if objective i has to be maximized
    val isMax = objVarMode.map(_._2).toArray
    
    stateObjective = Unit => {
      recordNonDominatedSolutions = true
      objective = new CPObjective(this, (for(i <- 0 until isMax.size) yield {
        if (isMax(i)) new CPObjectiveUnitMaximize(objectives(i))
        else new CPObjectiveUnitMinimize(objectives(i))
      }):_*)
      postCut(objective)
      objective.objs.foreach(_.tightenMode = TightenType.NoTighten)
    }
    
    addDecisionVariables(objectives)
    paretoSet = new ListPareto[CPSol](isMax)
    // Adds a dominance constraint with all objectives in minimization mode
    addCut(new ParetoConstraint(paretoSet, isMax, objectives.toArray))
    this
  }

  def solve(): CPSolver = {
    this
  }

  def subjectTo(constraintsBlock: => Unit): CPSolver = {
    try {
      constraintsBlock
      stateObjective()
      pushState()
      deactivateNoSolExceptions()
    } catch {
      case ex: NoSolutionException => println("No Solution, inconsistent model")
    }
    this
  }

  /**
   * return true if every variable is bound
   */
  def allBounds(vars: IndexedSeq[CPVarInt]) = vars.map(_.isBound).foldLeft(true)((a, b) => a & b)

  def minDom(x: CPVarInt): Int = x.size
  def minRegret(x: CPVarInt): Int = x.max - x.min
  def minDomMaxDegree(x: CPVarInt): (Int, Int) = (x.size, -x.constraintDegree)
  def minVar(x: CPVarInt): Int = 1
  def maxDegree(x: CPVarInt): Int = -x.constraintDegree

  def minVal(x: CPVarInt): Int = x.min
  def maxVal(x: CPVarInt): Int = x.max
  def minValminVal(x: CPVarInt): (Int, Int) = (x.min, x.min)

  /**
   * Deterministic branching
   * Binary First Fail (min dom size) on the decision variables vars. Ties are broken randomly
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def deterministicBinaryFirstFail(vars: Array[CPVarInt], valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
    binary(vars,x => (x.size,vars.indexOf(x)),valHeuris)
  }
  
  
  /**
   * Randomized branching
   * Binary First Fail (min dom size) on the decision variables vars. Ties are broken randomly
   * @param vars: the array of variables to assign during the search
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binaryFirstFail(vars: Array[CPVarInt], valHeuris: (CPVarInt => Int) = minVal): Unit @suspendable = {
    binary(vars,_.size,valHeuris)
  }
  
  /**
   * Deterministic branching:
   * Instantiate variable in from the first to last one in vars, trying smallest value first
   */
  def binary(vars: Array[_ <: CPVarInt]): Unit @suspendable = {
    binary(vars,vars.indexOf(_),minVal)
  }
  
  
  /**
   * Randomized branching (unless you use a deterministic varHeuris,valHeuris selection rules):
   * Binary search on the decision variables vars with custom variable/value heuristic (random tie breaking)
   * @param vars: the array of variables to assign during the search
   * @param varHeuris: for each variable, it's priority. 
   *        The non-instantiated variable with the smallest priority is chosen first (random tie break).
   * 		Note that a tuple can be used as variable priority to get lexicographical tie breaking rule.
   * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
   */
  def binary[T](vars: Array[_ <: CPVarInt], varHeuris: (CPVarInt => T), valHeuris: (CPVarInt => Int) = minVal)(implicit orderer: T => Ordered[T]): Unit @suspendable = {
    while (!allBounds(vars)) {
      val x = selectMin(vars.asInstanceOf[Array[CPVarInt]])(!_.isBound)(varHeuris).get
      val v = valHeuris(x)
      branch(post(x == v))(post(x != v)) // right alternative
    }
  }


  def binaryFirstFail(vars: CPVarInt*): Unit @suspendable = {
    binary(vars.toArray,_.size,minVal)
  }

  /**
   * Binary search on the decision variables vars, selecting first the variables having the max number
   * of propagation methods attached to it.
   */
  def binaryMaxDegree(vars: Array[CPVarInt]): Unit @suspendable = {
    binary(vars, varHeuris = maxDegree, valHeuris = minVal)
  }

  /**
   * Binary search on the decision variables vars, splitting the domain of the selected variable on the
   * median of the values (left : <= median, right : > median)
   */
  def binaryDomainSplit(vars: Array[CPVarInt], varHeuris: (CPVarInt => Int) = minVar, valHeuris: (Int => Int) = i => i): Unit @suspendable = {

    while (!allBounds(vars)) {

      val unbound = vars.filter(!_.isBound)
      val heuris = unbound.map(varHeuris(_)).min
      val x = unbound.filter(varHeuris(_) == heuris).head

      val vals = x.toArray.sortBy(valHeuris)
      val median = vals(vals.size / 2)

      branch(post(x <= median))(post(x > median))
    }
  }

  override def update() = propagate()
  override def solFound() = {
    super.solFound()
    lastSol = new CPSol(decVariables.toSet)
    if (recordNonDominatedSolutions) {
      if (!silent) println("new solution:"+objective.objs.map(_.objVar.value).toArray.mkString(","))
      paretoSet.insert(lastSol, objective.objs.map(_.objVar.value):_*)
    }
    objective.tighten()
  }

  def printStats() {
    println("time(ms)", time)
    println("#bkts", bkts)
    println("time in fix point(ms)", timeInFixPoint)
    println("time in trail restore(ms)", getTrail().getTimeInRestore())
    println("max trail size", getTrail().getMaxSize())
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
