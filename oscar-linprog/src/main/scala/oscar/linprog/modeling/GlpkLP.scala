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

import org.gnu.glpk._
import org.gnu.glpk.GLPKJNI

/**
 * @author Hrayr Kostanyan Hrayr.Kostanyan@ulb.ac.be, Pierre Schaus pschaus@gmail.com
 */
class GlpkLP extends AbstractLP {
  println("Glpk")
  var lp: glp_prob = null
  var nbRows = 0
  var nbCols = 0
  var sol = Array[Double]()
  var objectiveValue = 0.0
  var status = LPStatus.NOT_SOLVED
  var closed = false
  var released = false
  var configFile = new java.io.File("ToBImplemented")
  protected var timeout = Int.MaxValue

  def startModelBuilding(nbRows: Int, nbCols: Int) {
    this.nbRows = 0
    this.nbCols = nbCols
    lp = GLPK.glp_create_prob()
    GLPK.glp_add_cols(lp, nbCols) //0 row, nbCols
  }

  def endModelBuilding() {
    closed = true
  }

  def setVarName(colId: Int, name: String) {
    // TODO implement
  }
  
  override def setTimeout(t: Int) {
    require(0 <= t)
    timeout = t
  }
  
  override def setName(name: String) {
    GLPK.glp_set_prob_name(lp, name)
  }

  def addConstraint(coef: Array[Double], col: Array[Int], rhs: Double, sign: String, name: String) {
    nbRows += 1
    //Adding a row giving a name
    GLPK.glp_add_rows(lp, 1)
    GLPK.glp_set_row_name(lp, nbRows, name)
    sign match {
      case "<=" =>
        GLPK.glp_set_row_bnds(lp, nbRows, GLPKConstants.GLP_UP, 0, rhs) //GLP_UP the 0 value will be ignored (col<=rhs)
      case ">=" =>
        GLPK.glp_set_row_bnds(lp, nbRows, GLPKConstants.GLP_LO, rhs, 0) // GLP_LO => the 0 value will be ignored (col>=rhs)
      case "==" =>
        GLPK.glp_set_row_bnds(lp, nbRows, GLPKConstants.GLP_FX, rhs, rhs) //GLP_FX => the bounds is fix (col=rhs)
    }

    // creating array of columns(integer) and putting the column number
    // i and glpk_column indices begin with 1 and col indice with 0 
    val cols = GLPK.new_intArray(col.size + 1)
    for (i <- 1 to col.size)
      GLPK.intArray_setitem(cols, i, col(i - 1) + 1)

    // creating array of coefficients(double) and putting the coefficient value
    val coefs = GLPK.new_doubleArray(col.size + 1)
    for (i <- 1 to col.size)
      GLPK.doubleArray_setitem(coefs, i, coef(i - 1))

    // adding the value of columns with corresponding coefficients 
    GLPK.glp_set_mat_row(lp, nbRows, col.size, cols, coefs)

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

    GLPK.glp_set_obj_name(lp, "objective")
    GLPK.glp_set_obj_dir(lp, if (minMode) GLPKConstants.GLP_MIN else GLPKConstants.GLP_MAX)

    // i and glpk_column indices begin with 1 and col indice with 0 
    for (i <- 0 until col.size)
      GLPK.glp_set_obj_coef(lp, col(i) + 1, coef(i))
  }

  def addColumn(obj: Double, row: Array[Int], coef: Array[Double]) {
    if (!closed) {
      println("cannot add a column in a non closed solver")
    } else {
      //adding a column without bounds (by default the column's bound is fix and equal to 0)
      nbCols += 1
      GLPK.glp_add_cols(lp, 1)
      GLPK.glp_set_col_bnds(lp, nbCols, GLPKConstants.GLP_LO, 0, 0)

      // creating array of columns(integer) and putting the column number
      // i and glpk_column indices begin with 1 and col indice with 0 
      val rows = GLPK.new_intArray(row.size + 1)
      for (i <- 1 to row.size)
        GLPK.intArray_setitem(rows, i, row(i - 1) + 1)

      // creating array of coefficients(double) and putting the coefficient value
      val coefs = GLPK.new_doubleArray(row.size + 1)
      for (i <- 1 to row.size)
        GLPK.doubleArray_setitem(coefs, i, coef(i - 1))

      // adding the value of column with corresponding coefficients 
      GLPK.glp_set_mat_col(lp, nbCols, row.size, rows, coefs)

      // adding objective
      GLPK.glp_set_obj_coef(lp, nbCols, obj)
    }
  }

  def getLowerBound(colId: Int): Double = {
    GLPK.glp_get_col_lb(lp, colId + 1)
  }

  def getUpperBound(colId: Int): Double = {
    GLPK.glp_get_col_ub(lp, colId + 1)
  }

  def updateLowerBound(colId: Int, lb: Double) {
    // GLPK does not ahve update method si it will put double bound with the same upperBound
    GLPK.glp_set_col_bnds(lp, colId + 1, GLPKConstants.GLP_DB, lb, getUpperBound(colId + 1))
  }

  def updateUpperBound(colId: Int, ub: Double) {
    // GLPK does not have update method  it will put double bound with the same lowerBound
    GLPK.glp_set_col_bnds(lp, colId + 1, GLPKConstants.GLP_DB, getLowerBound(colId + 1), ub)
  }

  def solveModel(): LPStatus.Value = {
    //GLPK.glp_write_lp(lp,null,"model.lp")
    val parm = new glp_smcp()
    GLPK.glp_init_smcp(parm)
    parm.setMsg_lev(GLPKConstants.GLP_MSG_ERR)
    if(timeout < Int.MaxValue) {
    	parm.setTm_lim(timeout)
    }

    val ret = GLPK.glp_simplex(lp, parm)
    GLPK.glp_get_status(lp) match {
      case GLPKConstants.GLP_UNBND => LPStatus.UNBOUNDED
      case GLPKConstants.GLP_INFEAS => LPStatus.INFEASIBLE
      case GLPKConstants.GLP_OPT => {
        sol = Array.tabulate(nbCols)(col => GLPK.glp_get_col_prim(lp, col + 1))
        objectiveValue = GLPK.glp_get_obj_val(lp)
        LPStatus.OPTIMAL
      }
      case _ => {
        sol = Array.tabulate(nbCols)(col => GLPK.glp_get_col_prim(lp, col + 1))
        objectiveValue = GLPK.glp_get_obj_val(lp)
        LPStatus.SUBOPTIMAL
      }
    }
  }

  def getValue(colId: Int): Double = {
    if (sol == null || colId < 0 || colId >= nbCols)
      0.0
    else
      sol(colId)
  }

  def getObjectiveValue(): Double = {
    objectiveValue
  }

  def setInteger(colId: Int) {
    GLPK.glp_set_col_kind(lp, colId + 1, GLPKConstants.GLP_IV)
  }

  def setFloat(colId: Int) {
    GLPK.glp_set_col_kind(lp, colId + 1, GLPKConstants.GLP_CV)
  }

  def setBounds(colId: Int, low: Double, up: Double) {
    if (low < up)
      GLPK.glp_set_col_bnds(lp, colId + 1, GLPKConstants.GLP_DB, low, up)
    else
      GLPK.glp_set_col_bnds(lp, colId + 1, GLPKConstants.GLP_FX, low, up)

  }

  def setUnboundUpperBound(colId: Int) {
    // the upper bound(0) will be ignored
    GLPK.glp_set_col_bnds(lp, colId + 1, GLPKConstants.GLP_LO, getLowerBound(colId + 1), 0)
  }

  def setUnboundLowerBound(colId: Int) {
    // the lower bound(0) will be ignored
    GLPK.glp_set_col_bnds(lp, colId + 1, GLPKConstants.GLP_UP, 0, getUpperBound(colId + 1))
  }

  def getReducedCost(colId: Int): Double = {
    if (colId < 0 || colId >= nbCols) {
      return 0.0
    } else {
      GLPK.glp_get_col_dual(lp, colId + 1)
    }
  }

  def getDual(rowId: Int): Double = {
    if (rowId < 0 || rowId >= nbRows) {
      return 0.0
    } else {
      GLPK.glp_get_row_dual(lp, rowId + 1)
    }
  }

  def deleteConstraint(rowId: Int) {
    val num = GLPK.new_intArray(1)
    GLPK.intArray_setitem(num, 1, rowId + 1)
    GLPK.glp_del_rows(lp, 1, num)
    nbRows -= 1
  }

  def addVariable() {
    GLPK.glp_add_cols(lp, 1)
  }

  def deleteVariable(colId: Int) {
    val num = GLPK.new_intArray(1)
    GLPK.intArray_setitem(num, 1, colId)
    GLPK.glp_del_cols(lp, 1, num)
    nbCols -= 1
  }
  
  def exportModel(fileName: String) {
    GLPK._glp_lpx_write_cpxlp(lp, fileName)
  }

  def release() {
    GLPK.glp_delete_prob(lp)
  }

  def updateRhs(consId: Int, rhs: Double): Unit = {
    println("Warning: updateRhs method is not implemented for GLPK")
  }

  def updateCoef(consId: Int, varId: Int, coeff: Double): Unit = {
    println("Warning: updateCoef method is not implemented for GLPK")
  }
}