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

package oscar.linprog.modeling

import oscar.algebra._

import scala.collection._

object LPStatus extends Enumeration {
    val NOT_SOLVED = Value("not solved yet")
    val OPTIMAL = Value("optimal")
    val SUBOPTIMAL = Value("suboptimal")
    val UNBOUNDED = Value("infeasible")
    val INFEASIBLE = Value("unbounded")
}


/**
 * Abstract class that must be extended to define a new LP solver
 */
abstract class AbstractLP {

	/**
	 * Number of rows in the model
	 */
  	var nbRows : Int
  	
  	/**
  	 * Number of columns / variables in the model
  	 */
  	var nbCols : Int
  	
  	/**
  	 * Solution, one entry for each column / variable
  	 */
    var sol : Array[Double]
  	
  	/**
  	 * Objective value 
  	 */
  	var objectiveValue : Double
 	
  	
  	var released : Boolean
	
  	
	def startModelBuilding(nbRows : Int,nbCols : Int)
	
  	/**
  	 * Terminates the model building, the model cannot be modified afterwards
  	 */
  	def endModelBuilding()
  	
  	/**
  	 * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] >= rhs'' to the model.
  	 * @param coef are the coefficients of the linear term
  	 * @param col indicates to which column/variable the coefficients refer to
  	 */
    def addConstraintGreaterEqual(coef : Array[Double], col : Array[Int], rhs : Double)
    

  	/**
  	 * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] <= rhs'' to the model.
  	 * @param coef are the coefficients of the linear term
  	 * @param col indicates to which column/variable the coefficients refer to
  	 */
  	def addConstraintLessEqual(coef : Array[Double], col : Array[Int], rhs : Double)

  	/**
  	 * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] == rhs'' to the model.
  	 * @param coef are the coefficients of the linear term
  	 * @param col indicates to which column/variable the coefficients refer to
  	 */  	
  	def addConstraintEqual(coef : Array[Double], col : Array[Int], rhs : Double)
  	
  	/**
  	 * Define the objective function as ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)]'' to the model.
  	 * @param coef are the coefficients of the linear term
  	 * @param col indicates to which column/variable the coefficients refer to
  	 * @param minMode = true if this objective should be minimized (default), false to maximize it 
  	 */
    def addObjective(coef : Array[Double], col : Array[Int], minMode : Boolean = true)
  	
  	/**
  	 * Add a column to the problem
  	 */
  	def addColumn(obj : Double, row : Array[Int], coef : Array[Double])
  	
  	def getLowerBound(colId : Int) : Double
  	
  	def getUpperBound(colId : Int) : Double
  	
  	def updateLowerBound(colId : Int, lb : Double) 
    
  	def updateUpperBound(colId : Int, ub : Double)
    
  	def solveModel(): LPStatus.Value
    
  	def getValue(colId : Int) : Double
  	
  	def getObjectiveValue() : Double
  	
  	def setBounds(colId : Int, low : Double, up : Double)
  	
  	def setUnboundUpperBound(colId : Int)
  	
  	def setUnboundLowerBound(colId : Int)
  	
  	def deleteConstraint(rowId : Int)
  	
  	def addVariable()
  	
  	def deleteVariable(colId : Int)
  	
  	def exportModel(fineName: String)
  	
  	/**
  	 * Release the memory of this solver
  	 */
  	def release()

  	def setIntParameter(name : String, value : Int) {println("not implemented")}
  	
  	def setFloatParameter(name : String, value : Double) {println("not implemented")}
  	
  	def setStringParameter(name : String, value : String) {println("not implemented")}
  	
  	def releaseMemory() {println("not implemented")}
  	
  	/**
  	 * Get the dual variable of the row/constraint
  	 */
  	def getDual(rowId : Int) : Double
  	
  	/**
	 * Get the reduced cost corresponding to the column/variable
	 */
	def getReducedCost(colId : Int) : Double
	
	/**
	 * Set the column/variable as an integer variable
	 */
  	def setInteger(colId : Int)
  	
  	/**
  	 * Set the column/variable as a float variable
  	 */
  	def setFloat(colId : Int)
}

// ----------------------------------------------------------------


/**
 * Trait used for the modeling of single objective minimization using DFO
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
trait AbstractLPModel extends Algebra {
  
  
  object LPSolverLib extends Enumeration {
    val lp_solve = Value("lp_solve")
    val cplex = Value("cplex")
    val glpk = Value("glpk")
    val gurobi = Value("gurobi")
  }
  


	/**
	 * Defines an Float unbounded variable in the LP solver with domain:
	 * [0,+inf] is unbounded = true, 
	 * [-inf,+inf] otherwise
	 */
  class AbstractLPVar(val solver: AbstractLPSolver, varName: String, lbound: Double, ubound: Double, doubleUnbounded: Boolean) extends Var {
    
	// do not swap next two lines
	val index = solver.register(this)

	val lb = lbound
	val ub = ubound
	var unbounded = doubleUnbounded // unbounded [-inf,+inf]
	var integer : Boolean = false


	def name = varName
	
	/**
	 * Adjust the bound of the variable and re-optimize
	 */
	def setBounds(lb: Double, ub: Double,reoptimize: Boolean = false) {
	  solver.setBounds(this,lb,ub,reoptimize)
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
    
    def getValue : Double = {
	  value match {
	    case Some(v) => v
	    case _ => Double.NegativeInfinity
	  }
	}
    
    /**
     * Return true if the variable is integer, false otherwise
     */
    def isInteger() : Boolean = integer   
    
    /**
     * Return true if the variable is unbounded, false otherwise
     */
    def isUnbounded() : Boolean = unbounded
    
    /**
     * Return the lower bound of the variable
     */
    def getLowerBound() : Double = lb
    
    /**
     * Return the upper bound of the variable
     */
    def getUpperBound() : Double = ub
    
    /**
     * Return the name of the variable
     */
    def getName() : String = name    

  }

  class LPConstraint(val solver : AbstractLPSolver,val cstr : LinearConstraint, val index: Int) {
    val e = cstr.linExpr.coef.toList
 	val coef : Array[Double] = e.map(_._2).toArray
	val varIds : Array[Int] =  e.map(_._1.index).toArray
    var rhs : Double = -cstr.linExpr.cte // value of the constant (minus because it goes from lhs to rhs)
  
    def getSolver() : AbstractLPSolver = solver

    def getSize() : Int = coef.length
    def getCst() : Double = rhs
  
    def getCoefs() = coef
    def getVarIds() = varIds
  
    def getDual() = solver.getDual(this)
	
  } 
  
  abstract class AbstractLPSolver {
         
    // map from the index of variables to their implementation
    protected val vars = mutable.HashMap.empty[Int,AbstractLPVar]
    private val cons = mutable.HashMap.empty[Int,LPConstraint]
    private val solution = mutable.HashMap.empty[Int,Double]
    protected var objective: LinearExpression = 0
    protected var minimize = true
    
    val solver: AbstractLP
    
    var status = LPStatus.NOT_SOLVED
 
    
    def register(vari: AbstractLPVar): Int = {
      vars(vars.size) = vari
      vars.size-1
    }
    
    def add(constr : LinearConstraint): LPConstraint = {
      val constraint = new LPConstraint(this,constr,cons.size)
      cons(cons.size) = constraint
      constraint
    }
    
    /**
     * add the constraints really into the solver implem
     */
    def addAllConstraints() {
      cons  foreach { case (i,c) =>
        c.cstr.consType match {
              case ConstraintType.GQ => solver.addConstraintGreaterEqual(c.coef,c.varIds,c.rhs)
              case ConstraintType.LQ => solver.addConstraintLessEqual(c.coef,c.varIds,c.rhs)
              case ConstraintType.EQ => solver.addConstraintEqual(c.coef,c.varIds,c.rhs)
        }
      }
    }
    
    def getValue(varIndex: Int): Option[Double] = solution.get(varIndex)
    
	def getDual(cons: LPConstraint) : Double = {
		solver.getDual(cons.index)
	}  
	
	def setVarProperties() = {
	  vars  foreach { case (i,x) =>
	    setVarBounds(x)
	  }
	}
	
	def setVarBounds(x: AbstractLPVar,reoptimize: Boolean = false) = {
	    if (x.isUnbounded) {
			solver.setUnboundUpperBound(x.index)
			solver.setUnboundLowerBound(x.index)
		} else {
			solver.setBounds(x.index, x.lb , x.ub)
		}
	    if (reoptimize) solveModel()
	}
	
	def setBounds(x: AbstractLPVar, lb: Double, ub: Double,reoptimize: Boolean = false) = {
		solver.setBounds(x.index,lb,ub)
	    if (reoptimize) solveModel()
	}
	
	
	def optimize(linExpr: LinearExpression, minimize : Boolean) : AbstractLPSolver = {
    	objective = linExpr
    	this.minimize = minimize
	    this
	}
	
	def minimize(expr: LinearExpression) : AbstractLPSolver = {
		optimize(expr,true)
	}
	
	def maximize(expr: LinearExpression) : AbstractLPSolver = {
		optimize(expr,false)
	}
	
	def subjectTo (constraintsBlock : => Unit) {
		//executes the block containing the constraints
		constraintsBlock
		

		solver.startModelBuilding(0,vars.size)
		setVarProperties() //set the the var bounds correctly
		val e = objective.coef.toList
 		val coef : Array[Double] = e.map(_._2).toArray
		val varIds : Array[Int] =  e.map(_._1.index).toArray
		
		solver.addObjective(coef, varIds, minimize)
		addAllConstraints()
		
		
		//close the model and optimize
		solveModel()
	}
	
	def suchThat (constraints: LinearConstraint*) {
		// add all the constraints
		constraints.foreach(add(_))
		//close the model and optimize
		solveModel()
	}
	
	
	def solveModel() {
		solver.endModelBuilding()
		status = solver.solveModel()
		if ((status == LPStatus.OPTIMAL) || (status == LPStatus.SUBOPTIMAL)) {
			(0 until vars.size) foreach {i =>  solution(i) = solver.getValue(i)}
		}
	}
	
	def getObjectiveValue() : Double = {
			objective.value match {
				case Some(v) => v
				case None => {
				  0 
				}	
			}
	}
	

	def getStatus() : LPStatus.Value = status
	
	def release() = solver.release()
	
	
  } // end class AbstractLPSolver

} // end of trait







