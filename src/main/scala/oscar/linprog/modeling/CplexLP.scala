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

import ilog.concert._
import ilog.concert.IloRange
import ilog.cplex._



/**
 * @author Hrayr Kostanyan Hrayr.Kostanyan@ulb.ac.be, Pierre Schaus pschaus@gmail.com
 */
class CplexLP extends AbstractLP {
  println("Using CPLEX")

  var nbRows = 0
  var nbCols = 0
  var sol = Array[Double]()
  var objectiveValue = 0.0
  var status = LPStatus.NOT_SOLVED
  var closed = false
  var released = false  
  val model = new IloCplex()
  val lp = model.addLPMatrix() // matrix associated to model
  var configFile = new java.io.File("ToBImplemented")

  var lexpr = model.linearNumExpr()

  def startModelBuilding(nbRows: Int, nbCols: Int) {
    
    this.nbCols = nbCols

    model.numVarArray(model.columnArray(lp, nbCols), 0.0, java.lang.Double.MAX_VALUE) // creating nbCols variables
  }

  def endModelBuilding() {
    println("Model has " + nbRows + " rows and " + nbCols + " columns.")
    closed = true
  }

  def setVarName(colId: Int, name: String) {
    lp.getNumVar(colId).setName(name);
  }

  def addConstraint(coef: Array[Double], col: Array[Int], rhs: Double, sign: String, name: String) {
     // do not use lp.addrow(model.addLe(...)). This is highly ineficient 
     val idx = sign match {
      case "<=" => lp.addRow(-Double.MaxValue,rhs,col,coef)
      case ">=" => lp.addRow(rhs,Double.MaxValue,col,coef)
      case "==" => lp.addRow(rhs,rhs,col,coef)
     }
     // range constructor used above does not allow to set name directly...
     lp.getRange(idx).setName(name)
    
     nbRows += 1
  }

  def addConstraintGreaterEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    addConstraint(coef, col, rhs, ">=", name)
  }

  def addConstraintLessEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    addConstraint(coef, col, rhs, "<=", name)

  }

  def addConstraintEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String) {
    addConstraint(coef, col, rhs, "==", name)
  }

  def addObjective(coef: Array[Double], col: Array[Int], minMode: Boolean = true) {
    val lin = model.linearNumExpr()
    for (i <- 1 to col.size)
      lin.addTerm(coef(i - 1), lp.getNumVar(col(i - 1)))
    lexpr = lin
    if (minMode) model.addMinimize(lin) else model.addMaximize(lin)
  }

  def addColumn(obj: Double, row: Array[Int], coef: Array[Double]) {
    if (!closed) {
      println("cannot add a column in a non closed solver")
    } else {
      nbCols += 1

      val variable = model.numVar(0, java.lang.Double.MAX_VALUE, IloNumVarType.Float)
      var col = lp.addColumn(variable, row, coef)

      lexpr.addTerm(variable, obj)
      model.getObjective().setExpr(lexpr)
    }
  }

  def getLowerBound(colId: Int): Double = {
    lp.getNumVar(colId).getLB()
  }

  def getUpperBound(colId: Int): Double = {
    lp.getNumVar(colId).getUB()
  }

  def updateLowerBound(colId: Int, lb: Double) {
    lp.getNumVar(colId).setLB(lb)
  }

  def updateUpperBound(colId: Int, ub: Double) {
    lp.getNumVar(colId).setUB(ub)
  }

  def solveModel(): LPStatus.Value = {
    println("solve mode========================")
    val res = model.solve()

    def extractSol() = {
      objectiveValue = model.getObjValue()
      sol = Array.tabulate(nbCols)(col => model.getValue(lp.getNumVar(col)))
    }

    model.getStatus() match {
      case IloCplex.Status.Optimal => {
        extractSol()
        LPStatus.OPTIMAL
      }
      case IloCplex.Status.Infeasible => {
        LPStatus.INFEASIBLE
      }
      case IloCplex.Status.Unbounded => {
        LPStatus.UNBOUNDED
      }
      case IloCplex.Status.InfeasibleOrUnbounded => {
        LPStatus.UNBOUNDED
      }
      case _ => {
        extractSol()
        LPStatus.SUBOPTIMAL
      }
    }
  }

  def getValue(colId: Int): Double = {
    if (sol == null || colId < 0 || colId >= nbCols)
      0.0
    else {
      sol(colId)
    }
  }

  def getObjectiveValue(): Double = {
    objectiveValue
  }

  def setInteger(colId: Int) {
    model.add(model.conversion(lp.getNumVar(colId), IloNumVarType.Int))
  }

  def setFloat(colId: Int) {
    // float variables by default
  }

  def setBounds(colId: Int, low: Double, up: Double) {
    lp.getNumVar(colId).setUB(up)
    lp.getNumVar(colId).setLB(low)
  }

  def setUnboundUpperBound(colId: Int) {
    // unbounded by default
  }

  def setUnboundLowerBound(colId: Int) {
    lp.getNumVar(colId).setLB(-java.lang.Double.MAX_VALUE)

  }

  def getReducedCost(colId: Int): Double = {
    if (colId < 0 || colId >= nbCols) {
      return 0.0
    } else {
      model.getReducedCost(lp.getNumVar(colId))
    }
  }

  def getDual(rowId: Int): Double = {
    model.getDual(lp.getRange(rowId))
  }

  def deleteConstraint(rowId: Int) {
    lp.removeRow(rowId)
   }

  def exportModel(fileName: String) {
    model.exportModel(fileName)
  }

  def addVariable() {
    val col = model.column(lp)
    model.numVar(col, 0, java.lang.Double.MAX_VALUE, IloNumVarType.Float)
  }

  def deleteVariable(colId: Int) {
    model.remove(lp.getNumVar(colId))
  }

  def release() {
    model.end()
  }
}
