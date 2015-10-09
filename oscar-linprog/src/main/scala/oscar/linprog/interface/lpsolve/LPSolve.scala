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

package oscar.linprog.interface.lpsolve

import java.nio.file.Path

import _root_.lpsolve.LpSolve
import oscar.linprog.interface._
import oscar.linprog.modeling.NotSolvedYet

class LPSolve extends MPSolverInterface {
  // NOTE:
  // variables (columns) and constraints (rows) indices of LpSolve are 1 based

  type Solver = LpSolve

  private var rawSolverOpt: Option[Solver] = None

  private def solverInitialized: Boolean = rawSolverOpt.isDefined

  def rawSolver = rawSolverOpt.get

  private def initSolver() = {
    rawSolverOpt = Some(LpSolve.makeLp(nRows, nCols))

    // infinity in OscaR is represented by Double.PositiveInfinity
    rawSolver.setInfinite(Double.PositiveInfinity)

    // Initially optimize for row by row addition
    rawSolver.setAddRowmode(true)
  }

  private var nCols = 0
  private var nRows = 0

  def modelName = rawSolver.getLpName
  def modelName_=(value: String) = rawSolver.setLpName(value)

  private var pendingObj: (Array[Double], Array[Int], Boolean) = (Array(), Array(), true)

  def addObjective(coef: Array[Double], varId: Array[Int], minimize: Boolean = true): Unit = {
    pendingObj = (coef, varId, minimize)
  }

  def setObjCoef(varId: Int, coef: Double): Unit = rawSolver.setObj(varId + 1, coef)

  def setOptimizationDirection(minimize: Boolean): Unit = {
    if(solverInitialized) {
      if (minimize) rawSolver.setMinim()
      else rawSolver.setMaxim()
    } else {
      pendingObj = (pendingObj._1, pendingObj._2, minimize)
    }
  }

  private var pendingVars: Seq[(Int, String, Double, Double)] = Seq()

  private def setVarProperties(varId: Int, name: String, lb: Double, ub: Double) = {
    rawSolver.setColName(varId + 1, name)
    setVarLB(varId, lb)
    setVarUB(varId, ub)
  }

  def addVariable(name: String, lb: Double = Double.NegativeInfinity, ub: Double = Double.PositiveInfinity,
    objCoef: Option[Double] = None, cstrCoef: Option[Array[Double]] = None, cstrId: Option[Array[Int]] = None): Int =
    (objCoef, cstrCoef, cstrId) match {
      case (Some(oCoef), Some(cCoef), Some(cId)) =>
        // first arg is the length of the arrays
        // second arg is the array of the coefficients of the constraints (coef in position 0 is the coef for the objective)
        // third arg is the row numbers of the constraints that should be updated (row 0 is the objective)
        rawSolver.addColumnex(cCoef.length + 1, oCoef +: cCoef, 0 +: cId.map(_ + 1))
        val varId = this.nCols
        setVarProperties(varId, name, lb, ub)
        this.nCols += 1
        varId
      case (None, None, None) =>
        // Note: actual addition of the variable is delayed until the next updateModel
        val varId = this.nCols
        pendingVars = (varId, name, lb, ub) +: pendingVars
        this.nCols += 1
        varId
      case _ =>
        throw new IllegalArgumentException("Parameters objCoef, cstrCoef, cstrId should all be defined or none.")
    }

  def getVarLB(varId: Int): Double = rawSolver.getLowbo(varId + 1)
  def setVarLB(varId: Int, lb: Double) = rawSolver.setLowbo(varId + 1, lb)

  def getVarUB(varId: Int): Double = rawSolver.getUpbo(varId + 1)
  def setVarUB(varId: Int, ub: Double) = rawSolver.setUpbo(varId + 1, ub)

  private var pendingCstrs: Seq[(Int, String, Array[Double], Array[Int], String, Double)] = Seq()

  private def addConstraintToModel(cstrId: Int, name: String, coef: Array[Double], varId: Array[Int], sense: String, rhs: Double) = {
    val sen = sense match {
      case "<=" => LpSolve.LE
      case "==" => LpSolve.EQ
      case ">=" => LpSolve.GE
      case _ => throw new IllegalArgumentException(s"Unexpected symbol for sense. Found: $sense. Expected: one of <=, == or >=.")
    }

    rawSolver.addConstraintex(coef.length, coef, varId.map(_ + 1), sen, rhs)
    rawSolver.setRowName(cstrId, name)
  }

  def addConstraint(name: String, coef: Array[Double], varId: Array[Int], sense: String, rhs: Double): Int = {
    val cstrId = this.nRows
    pendingCstrs = (cstrId, name, coef, varId, sense, rhs) +: pendingCstrs
    this.nRows += 1
    cstrId
  }

  def setCstrCoef(cstrId: Int, varId: Int, coef: Double): Unit = rawSolver.setMat(cstrId + 1, varId + 1, coef)
  def setCstrRhs(cstrId: Int, rhs: Double): Unit = rawSolver.setRh(cstrId + 1, rhs)

  def nVariables: Int = nCols
  def nLinearConstraints: Int = nRows

  def updateModel() = {
    if(!solverInitialized) initSolver()
    else rawSolver.resizeLp(nRows, nCols)

    // add the pending vars
    pendingVars foreach { case (varId, name, lb, ub) => setVarProperties(varId, name, lb, ub) }

    // add the pending objective
    val (oCoef, oVarId, oMin) = pendingObj
    rawSolver.setObjFnex(oCoef.length, oCoef, oVarId.map(_ + 1))
    setOptimizationDirection(oMin)

    // add the pending constraints
    pendingCstrs foreach { case (cstrId, name, coef, varId, sense, rhs) => addConstraintToModel(cstrId, name, coef, varId, sense, rhs) }

    rawSolver.setAddRowmode(false)
  }

  def exportModel(filepath: Path, format: ExportFormat): Unit =
    format match {
      case LP => rawSolver.writeLp(filepath.toString) // Note: this is lp_solve's own lp format which is different from CPLEX's one.
      case MPS => rawSolver.writeFreeMps(filepath.toString)
      case _ => println(s"Unrecognised export format $format")
    }

  private var _endStatus: Option[EndStatus] = None

  def endStatus: EndStatus = _endStatus match {
    case Some(es) => es
    case None => throw NotSolvedYet
  }

  def hasSolution: Boolean = endStatus == SolutionFound

  def objectiveValue: Double = rawSolver.getObjective

  def objectiveBound: Double = rawSolver.getObjBound

  def solution: Array[Double] = rawSolver.getPtrVariables

  def getVarValue(varId: Int): Double = solution(varId)

  def cstrSolution: Array[Double] = rawSolver.getPtrConstraints

  def getCstrValue(cstrId: Int): Double = cstrSolution(cstrId)

  def optimize(): EndStatus = {
    updateModel()

    val status = rawSolver.solve

    _endStatus = Some(status match {
      case LpSolve.OPTIMAL => SolutionFound
      case LpSolve.SUBOPTIMAL => SolutionFound
      case LpSolve.INFEASIBLE => Infeasible
      case LpSolve.UNBOUNDED => Unbounded
      case LpSolve.USERABORT => NoSolution
      case LpSolve.TIMEOUT => NoSolution
      case _ => NoSolution
    })

    endStatus
  }

  var _released = false

  def release(): Unit = {
    _released = true
    rawSolver.deleteLp()
  }

  def released: Boolean = _released

  def configure(absPath: Path) = rawSolver.readParams(absPath.toString, "[Default]")

  def setTimeout(t: Long) = rawSolver.setTimeout(t)
}
