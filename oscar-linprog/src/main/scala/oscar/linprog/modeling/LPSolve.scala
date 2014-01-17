/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.linprog.modeling

import lpsolve._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class LPSolve extends AbstractLP {
  println("LPSolve")
  var lp: LpSolve = null
  var nbRows = 0
  var nbCols = 0
  var sol = Array[Double]()
  var objectiveValue = 0.0
  var status = LPStatus.NOT_SOLVED
  var closed = false
  var released = false
  var configFile = new java.io.File("options.ini")

  def startModelBuilding(nbRows: Int, nbCols: Int) {
    this.nbRows = 0
    this.nbCols = nbCols
    lp = LpSolve.makeLp(0, nbCols) //0 row, nbCols
    lp.setInfinite(Double.MaxValue)
    lp.setAddRowmode(true)
    if (configFile.exists()) {
      lp.readParams(configFile.getAbsolutePath, "[Default]");
    }
  }

  def endModelBuilding() {
    lp.setAddRowmode(false)
    closed = true
  }

  def setVarName(colId: Int, name: String) {
	  lp.setColName(colId + 1, name)
  }

  def addConstraintGreaterEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    nbRows += 1
    lp.addConstraintex(coef.length, coef, col.map(_ + 1), LpSolve.GE, rhs) //the column index of lp_solve is 1 based
  }

  def addConstraintLessEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    nbRows += 1
    lp.addConstraintex(coef.length, coef, col.map(_ + 1), LpSolve.LE, rhs)
  }
  
  def addConstraintEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    nbRows += 1
    lp.addConstraintex(coef.length, coef, col.map(_ + 1), LpSolve.EQ, rhs)
  }

  def addObjective(coef: Array[Double], col: Array[Int], minMode: Boolean = true) {
    lp.setObjFnex(coef.length, coef, col.map(_ + 1))
    if (!minMode) {
      lp.setMaxim()
    }
  }

  def addColumn(obj: Double, row: Array[Int], coef: Array[Double]) {
    if (!closed) {
      println("cannot add a column in a non closed solver")
    } else {
      lp.addColumnex(coef.length + 1, coef :+ obj, row.map(_ + 1) :+ 0)
      this.nbCols += 1
    }
  }

  def getLowerBound(colId: Int): Double = {
    lp.getLowbo(colId + 1)
  }

  def getUpperBound(colId: Int): Double = {
    lp.getUpbo(colId + 1)
  }

  def updateLowerBound(colId: Int, lb: Double) {
    lp.setLowbo(colId + 1, lb)
  }

  def updateUpperBound(colId: Int, ub: Double) {
    lp.setUpbo(colId + 1, ub)
  }

  def solveModel(): LPStatus.Value = {

    val status = lp.solve match {
      case LpSolve.OPTIMAL =>
        sol = Array.tabulate(nbCols)(c => lp.getVarPrimalresult(nbRows + c + 1))
        objectiveValue = lp.getObjective()
        LPStatus.OPTIMAL
      case LpSolve.SUBOPTIMAL =>
        sol = Array.tabulate(nbCols)(c => lp.getVarPrimalresult(nbRows + c + 1))
        objectiveValue = lp.getObjective()
        LPStatus.SUBOPTIMAL
      case LpSolve.INFEASIBLE =>
        LPStatus.INFEASIBLE
      case LpSolve.UNBOUNDED =>
        LPStatus.UNBOUNDED
      case _ =>
        LPStatus.INFEASIBLE
    }
    if (status == LpSolve.OPTIMAL) {
      println("-------  ssolving ----- " + status)
      println("nbcol now:" + lp.getNcolumns() + " orig columns:" + lp.getNorigColumns())
      println("nbrow now:" + lp.getNrows() + " orig rows:" + lp.getNorigRows())
    }
    status
  }

  def getValue(colId: Int): Double = {
    if (sol == null || colId < 0 || colId >= nbCols) {
      0.0
    } else {
      sol(colId)
    }
  }

  def getObjectiveValue(): Double = {
    objectiveValue
  }

  def setInteger(colId: Int) {
    lp.setInt(colId + 1, true)
  }

  def setFloat(colId: Int) {
    lp.setInt(colId + 1, false)
  }
  
  def setBounds(colId: Int, low: Double, up: Double) {
    lp.setBounds(colId + 1, low, up)
  }

  def setUnboundUpperBound(colId: Int) {
    lp.setUpbo(colId + 1, lp.getInfinite)
  }

  def setUnboundLowerBound(colId: Int) {
    lp.setLowbo(colId + 1, -lp.getInfinite)
  }

  def getReducedCost(colId: Int): Double = {
    if (colId < 0 || colId >= nbCols) {
      0.0
    } else {
      lp.getPtrDualSolution()(nbRows + colId + 1)
    }
  }

  def getDual(rowId: Int): Double = {
    if (rowId < 0 || rowId >= nbRows) {
      0.0
    } else {
      println( lp.getPtrDualSolution().mkString(", " ))
      lp.getPtrDualSolution()(rowId +1)
    }
  }

  def deleteConstraint(rowId: Int) {
    lp.delConstraint(rowId + 1)
  }

  def addVariable() {
    lp.addColumnex(0, null, null)
  }

  def deleteVariable(colId: Int) {
    lp.delColumn(colId)
  }

  def exportModel(fileName: String) {
    lp.writeLp(fileName)
  }

  def release() {
    lp.deleteLp()
  }

  def updateRhs(rowId: Int, rhs: Double): Unit = {
    lp.setRh(rowId + 1, rhs)
  }

  def updateCoef(rowId: Int, colId: Int, coeff: Double): Unit = {
    lp.setMat(rowId + 1, colId + 1, coeff)
  }
}