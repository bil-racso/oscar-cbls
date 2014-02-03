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

package oscar.linprog.modeling

import oscar.algebra._
import scala.collection._

object LPStatus extends Enumeration {
  val NOT_SOLVED = Value("not solved yet")
  val OPTIMAL = Value("optimal")
  val SUBOPTIMAL = Value("suboptimal")
  val UNBOUNDED = Value("unbounded")
  val INFEASIBLE = Value("infeasible")
}

object LPExportFormat extends Enumeration {
    val LP, MPS = Value
}

/**
 * Abstract class that must be extended to define a new LP solver
 */
abstract class AbstractLP {

  /**
   * Number of rows in the model
   */
  var nbRows: Int

  /**
   * Number of columns / variables in the model
   */
  var nbCols: Int

  /**
   * Solution, one entry for each column / variable
   */
  var sol: Array[Double]

  /**
   * Objective value
   */
  var objectiveValue: Double

  var released: Boolean

  var configFile: java.io.File

  def startModelBuilding(nbRows: Int, nbCols: Int)

  /**
   * Terminates the model building, the model cannot be modified afterwards
   */
  def endModelBuilding()

  /**
   *  Adds all the constraints to the model
   *  @param cons map of all the constraints with their id
   *
   *  Remark: defining this method in AbstractLP instead of AbstractLPSolver allows to overwrite it for different solvers
   *  if it has a method to add all the constraints at once (ex: Gurobi).
   */
  def addAllConstraints(cons: Map[Int, LPConstraint]) {
    var nbC = 0
    for {
      i <- cons.keys.toSeq.sortWith(_ < _)
      c = cons(i)
    } {
      if (nbC > 0 && nbC % 1000 == 0) println("Added " + nbC + " constraints. Currently at constraint index " + i)
        c match {
        	case c: SOSConstraint => addConstraintSOS1(c.varIds, c.weightings(), c.name)
        	case _ if(c.cstr.consType == ConstraintType.GQ) => addConstraintGreaterEqual(c.coef, c.varIds, c.rhs, c.name)
        	case _ if(c.cstr.consType == ConstraintType.LQ) => addConstraintLessEqual(c.coef, c.varIds, c.rhs, c.name)
        	case _ if(c.cstr.consType == ConstraintType.EQ) => addConstraintEqual(c.coef, c.varIds, c.rhs, c.name)
      	}
      nbC += 1
    }
  }

  /**
   * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] >= rhs'' to the model.
   * @param coef are the coefficients of the linear term
   * @param col indicates to which column/variable the coefficients refer to
   */
  def addConstraintGreaterEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String)

  /**
   * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] <= rhs'' to the model.
   * @param coef are the coefficients of the linear term
   * @param col indicates to which column/variable the coefficients refer to
   */
  def addConstraintLessEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String)
  
  /**
   * Add an Specially Ordered Set (SOS) Type 1 constraint. Constrains at most one variable 
   * in a collection to be equal to 1. Useful for modelling discrete choices
   * @param col indicates which variables are included in the SOS constraints 
   * @param ceof the weightings on the variables (these influence the search branching decision)
   */
  def addConstraintSOS1(col: Array[Int], coef: Array[Double] = null, name: String): Unit = ???
  
  /**
   * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] == rhs'' to the model.
   * @param coef are the coefficients of the linear term
   * @param col indicates to which column/variable the coefficients refer to
   */
  def addConstraintEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String)

  /**
   *  modify the right hand side (constant term) of the specified constraint
   */
  def updateRhs(consId: Int, rhs: Double): Unit

  /**
   *  Set the coefficient of the variable in the corresponding constraint to the specified value
   */
  def updateCoef(consId: Int, varId: Int, coeff: Double): Unit

  /**
   * Define the objective function as ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)]'' to the model.
   * @param coef are the coefficients of the linear term
   * @param col indicates to which column/variable the coefficients refer to
   * @param minMode = true if this objective should be minimized (default), false to maximize it
   */
  def addObjective(coef: Array[Double], col: Array[Int], minMode: Boolean = true)

  /**
   * Add a column to the problem
   */
  def addColumn(obj: Double, row: Array[Int], coef: Array[Double])

  def getLowerBound(colId: Int): Double

  def getUpperBound(colId: Int): Double

  def updateLowerBound(colId: Int, lb: Double)

  def updateUpperBound(colId: Int, ub: Double)

  def solveModel(): LPStatus.Value

  def getValue(colId: Int): Double

  def getObjectiveValue(): Double

  def setBounds(colId: Int, low: Double, up: Double)

  def setUnboundUpperBound(colId: Int)

  def setUnboundLowerBound(colId: Int)

  /** Set name of variable in solver */
  def setVarName(colId: Int, name: String)
  
  /** Set name of model in solver */
  def setName(name: String)

  def deleteConstraint(rowId: Int)

  def addVariable()

  def deleteVariable(colId: Int)

  def exportModel(fileName: String, format: LPExportFormat.Value = LPExportFormat.MPS)
  
  def setTimeout(t: Int)
  

  /**
   * Release the memory of this solver
   */
  def release()

  def setIntParameter(name: String, value: Int) { println("not implemented") }

  def setFloatParameter(name: String, value: Double) { println("not implemented") }

  def setStringParameter(name: String, value: String) { println("not implemented") }

  def releaseMemory() { println("not implemented") }

  /**
   * Get the dual variable of the row/constraint
   */
  def getDual(rowId: Int): Double

  /**
   * Get the reduced cost corresponding to the column/variable
   */
  def getReducedCost(colId: Int): Double

  /**
   * Set the column/variable as an integer variable
   */
  def setInteger(colId: Int)

  /**
   * Set the column/variable as a float variable
   */
  def setFloat(colId: Int)
}

// ----------------------------------------------------------------

/**
 * Trait used for the modeling of single objective minimization using LP-MIP
 *
 * @author Pierre Schaus pschaus@gmail.com
 */

/**
 * Defines an Float unbounded variable in the LP solver with domain:
 * [0,+inf] is unbounded = true,
 * [-inf,+inf] otherwise
 */
class AbstractLPFloatVar(val solver: AbstractLPSolver, varName: String, lbound: Double, ubound: Double, doubleUnbounded: Boolean) extends Var {

  // do not swap next two lines
  val index = solver.register(this)

  val lb = lbound
  val ub = ubound
  var unbounded = doubleUnbounded // unbounded [-inf,+inf]
  var integer: Boolean = false

  def name = varName

  /**
   * Adjust the bound of the variable and re-optimize
   */
  def setBounds(lb: Double, ub: Double, reoptimize: Boolean = false) {
    solver.setBounds(this, lb, ub, reoptimize)
  }

  /**
   * Reset initial bounds and repotimize
   */
  def resetBounds() {
    solver.setVarProperties()
  }

  /**
   * Returns the value of the variable (integer rounded if the variable is integer)
   */
  override def value = solver.getValue(index)

  /**
   * Return true if the variable is integer, false otherwise
   */
  def isInteger(): Boolean = integer

  /**
   * Return true if the variable is unbounded, false otherwise
   */
  def isUnbounded(): Boolean = unbounded

  /**
   * Return the lower bound of the variable
   */
  def getLowerBound(): Double = lb

  /**
   * Return the upper bound of the variable
   */
  def getUpperBound(): Double = ub

  /**
   * Return the name of the variable
   */
  def getName(): String = name

}

class LPConstraint(val solver: AbstractLPSolver, val cstr: LinearConstraint, val index: Int, val name: String) {

  private val e = cstr.linExpr.coef.toArray
  val perm = (0 until e.size).sortBy(i => e(i)._1.index)

  val coef: Array[Double] = perm.map(i => e(i)._2).toArray
  val varIds: Array[Int] = perm.map(i => e(i)._1.index).toArray
  val rhs: Double = -cstr.linExpr.cte // value of the constant (minus because it goes from lhs to rhs)
  def getSolver(): AbstractLPSolver = solver

  def size(): Int = coef.length

  def dual() = solver.getDual(this)

  def check(tol: Double = 10e-6): Boolean = {
    val s = slack()
    cstr.consType match {
      case ConstraintType.GQ => s + tol >= 0
      case ConstraintType.LQ => s + tol >= 0
      case ConstraintType.EQ => s.abs - tol <= 0
    }
  }

  def slack(): Double = {
    var res = 0.0
    val ex = cstr.linExpr.coef.toArray

    for ((i, a) <- varIds.zip(coef)) {
      val x: AbstractLPFloatVar = solver.variable(i) match {
        case Some(variable) => variable
        case None => throw new IllegalArgumentException("Variable with index " + i + " not present in lp solver")
      }
      res += a * x.value.get
    }
    cstr.consType match {
      case ConstraintType.GQ => res - rhs
      case ConstraintType.LQ => rhs - res
      case ConstraintType.EQ => rhs - res
    }
  }

  def isTight(tol: Double = 10e-6) = slack.abs <= tol
  override def toString: String = name + ": " + cstr
}

class SOSConstraint(override val solver: AbstractLPSolver, override val cstr: LinearConstraint, override val index: Int, override val name: String) extends LPConstraint(solver, cstr, index, name) {
	
	def weightings() = coef
	def getSOSType() = rhs
}


abstract class AbstractLPSolver {

  // map from the index of variables to their implementation
  protected val vars = mutable.HashMap.empty[Int, AbstractLPFloatVar]
  private val cons = mutable.HashMap.empty[Int, LPConstraint]
  private val solution = mutable.HashMap.empty[Int, Double]
  protected var objective: LinearExpression = 0
  protected var minimize = true
 
  protected val solver: AbstractLP

  protected var statuss = LPStatus.NOT_SOLVED
  protected var modelName = "" 
 
  def name_= (n: String): Unit = modelName = n
  def name: String = modelName
  
  def register(vari: AbstractLPFloatVar): Int = {
    vars(vars.size) = vari
    vars.size - 1
  }

  /**
   * @deprecated("no need to use option",1.0)
   */
  def add(constr: LinearConstraint, name: Option[String]): LPConstraint = {
    name match {
      case None => add(constr, "")
      case Some(n) => add(constr, n)
    }
  }

  def add(constr: LinearConstraint, name: String = ""): LPConstraint = {
    val cstName = if (name.isEmpty()) "cstr" + cons.size else name
    val constraint = new LPConstraint(this, constr, cons.size, cstName)
    cons(cons.size) = constraint
    constraint
  }
  
  def addSOS(constr: LinearConstraint, name: String = ""): SOSConstraint = {
    val cstName = if (name.isEmpty()) "SOS_" + cons.size else name
    val constraint = new SOSConstraint(this, constr, cons.size, cstName)
    cons(cons.size) = constraint
    constraint
  }

  def variable(i: Int) = vars.get(i)

  /**
   * add the constraints really into the solver implem
   */
  def addAllConstraints() {
    solver.addAllConstraints(cons)
  }

  def getValue(varIndex: Int): Option[Double] = solution.get(varIndex)

  def getDual(cons: LPConstraint): Double = {
    solver.getDual(cons.index)
  }

  def setVarProperties() = {
    vars foreach {
      case (i, x) =>
        setVarBounds(x)
        solver.setVarName(x.index, x.getName())
    }
  }

  def setVarBounds(x: AbstractLPFloatVar, reoptimize: Boolean = false) = {
    if (x.isUnbounded) {
      solver.setUnboundUpperBound(x.index)
      solver.setUnboundLowerBound(x.index)
    } else {
      solver.setBounds(x.index, x.lb, x.ub)
    }
    if (reoptimize) solveModel()
  }

  def setBounds(x: AbstractLPFloatVar, lb: Double, ub: Double, reoptimize: Boolean = false) = {
    solver.setBounds(x.index, lb, ub)
    if (reoptimize) solveModel()
  }

  def optimize(linExpr: LinearExpression, minimize: Boolean): AbstractLPSolver = {
    objective = linExpr
    this.minimize = minimize
    this
  }

  def minimize(expr: LinearExpression): AbstractLPSolver = {
    optimize(expr, true)
  }

  def maximize(expr: LinearExpression): AbstractLPSolver = {
    optimize(expr, false)
  }
 
  /**
   * Effectively start the concrete model building and solving
   * @param timeLimit (in seconds) to pass to the solver
   * @returns true if a feasible solution is found (optimal or sub-optimal)
   */
  def start(timeLimit: Int = Int.MaxValue): Boolean = {
    solver.startModelBuilding(cons.size, vars.size)
    solver.setName(modelName)
    println("Setting variable bounds...")
    setVarProperties() //set the the var bounds correctly
    val e = objective.coef.toList
    val coef: Array[Double] = e.map(_._2).toArray
    val varIds: Array[Int] = e.map(_._1.index).toArray

    println("Creating objective...")
    solver.addObjective(coef, varIds, minimize)

    println("Creating constraints...")
    val t0 = System.currentTimeMillis()
    addAllConstraints()
    println(s"Time to add constraints ${System.currentTimeMillis() - t0} ms")
    if(timeLimit < Int.MaxValue){
      solver.setTimeout(timeLimit)
    }
    
    solveModel()
    (status == LPStatus.OPTIMAL) || (status == LPStatus.SUBOPTIMAL)
  }


  def solveModel() {
    solver.endModelBuilding()
    println("Solving ...")
    statuss = solver.solveModel()
    if ((statuss == LPStatus.OPTIMAL) || (statuss == LPStatus.SUBOPTIMAL)) {
      (0 until vars.size) foreach { i => solution(i) = solver.getValue(i) }
    }
  }

  /**
   * @return The objective value (None if problem not yet solved or infeasible)
   */
  def objectiveValue() = objective.value

  def status(): LPStatus.Value = statuss

  def release() = solver.release()

  /**
   * Check that all the constraints are satisfied
   */
  def checkConstraints(tol: Double = 10e-6): Boolean = cons.values.forall(c => c.check(tol))

  /**
   *  modify the right hand side (constant term) of the specified constraint directly in the solver
   */
  def updateRhs(constraint: LPConstraint, rhs: Double) = {
    // update directly in the solver if the model has already been build
    if (statuss != LPStatus.NOT_SOLVED) solver.updateRhs(constraint.index, rhs)

    // update of the rhs in the oscar model
    val updatedExpr = constraint.cstr.linExpr - constraint.cstr.linExpr.cte - rhs
    val updatedCstr = new LinearConstraint(updatedExpr, constraint.cstr.consType)
    val updatedConstraint = new LPConstraint(this, updatedCstr, constraint.index, constraint.name)
    cons(constraint.index) = updatedConstraint

    updatedConstraint
  }

  /**
   *  Set the coefficient of the variable in the corresponding constraint to the specified value
   */
  def updateCoef(constraint: LPConstraint, variable: AbstractLPFloatVar, coeff: Double) = {
    // update directly in the solver if the model has already been build
    if (statuss != LPStatus.NOT_SOLVED) solver.updateCoef(constraint.index, variable.index, coeff)

    // update of the coeff in the oscar model
    val oldExpr = constraint.cstr.linExpr
    val updatedExpr = if (oldExpr.coef.keys.exists(_ == variable)) oldExpr - oldExpr.coef(variable) * variable + coeff * variable
    else oldExpr + coeff * variable
    val updatedCstr = new LinearConstraint(updatedExpr, constraint.cstr.consType)
    val updatedConstraint = new LPConstraint(this, updatedCstr, constraint.index, constraint.name)
    cons(constraint.index) = updatedConstraint

    updatedConstraint
  }
} // end class AbstractLPSolver









