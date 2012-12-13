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
    val UNBOUNDED = Value("unbounded")
    val INFEASIBLE = Value("infeasible")
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
  	
  	var configFile : java.io.File 
	
  	
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
  def addConstraintGreaterEqual(coef: Array[Double], col: Array[Int], rhs: Double, name: String)
     

  	/**
  	 * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] <= rhs'' to the model.
  	 * @param coef are the coefficients of the linear term
  	 * @param col indicates to which column/variable the coefficients refer to
  	 */
  	def addConstraintLessEqual(coef : Array[Double], col : Array[Int], rhs : Double, name: String)

  	/**
  	 * Add the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] == rhs'' to the model.
  	 * @param coef are the coefficients of the linear term
  	 * @param col indicates to which column/variable the coefficients refer to
  	 */  	
  	def addConstraintEqual(coef : Array[Double], col : Array[Int], rhs : Double, name: String)
  	
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
  	
  	/** Set name of variable in solver */
  	def setVarName(colId : Int, name: String)
  	
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
 * Trait used for the modeling of single objective minimization using LP-MIP
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */

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

class LPConstraint(val solver : AbstractLPSolver,val cstr : LinearConstraint, val index: Int, val name:String) {

 	private val e = Vector() ++ cstr.linExpr.coef.toList 
 	private val perm = (0 until e.size).sortBy(i => e(i)._1.index)
 	
    val coef : Array[Double] = perm.map(i => e(i)._2).toArray
    val varIds : Array[Int] =  perm.map(i => e(i)._1.index).toArray
    val rhs : Double = -cstr.linExpr.cte // value of the constant (minus because it goes from lhs to rhs)
    def getSolver() : AbstractLPSolver = solver

    def size() : Int = coef.length
   
    
    def dual() = solver.getDual(this)
	
} 
  
abstract class AbstractLPSolver {
         
    // map from the index of variables to their implementation
    protected val vars = mutable.HashMap.empty[Int,AbstractLPVar]
    protected val cons = mutable.HashMap.empty[Int,LPConstraint]
    private val solution = mutable.HashMap.empty[Int,Double]
    protected var objective: LinearExpression = 0
    protected var minimize = true
    /** Should solveModel be called at end of subjectTo and suchThat blocks?*/
    protected var autoSolve = true 
    
    protected val solver: AbstractLP
    
    protected var statuss = LPStatus.NOT_SOLVED
 
    
    def register(vari: AbstractLPVar): Int = {
      vars(vars.size) = vari
      vars.size-1
    }
    
    def add(constr : LinearConstraint,name: Option[String]= None): LPConstraint = {
      val cstName = name match {
        case Some(n) => n
        case None => "cstr"+cons.size
      }
      val constraint = new LPConstraint(this,constr,cons.size,cstName)
      cons(cons.size) = constraint
      constraint
    }
    
    /**
     * add the constraints really into the solver implem
     */
    def addAllConstraints() {
      var nbC = 0
      cons  foreach { case (i,c) =>
        if (nbC % 1000 == 0) println("Added "+nbC+" constraints. Currently at constraint index "+ i)
        c.cstr.consType match {
              case ConstraintType.GQ => solver.addConstraintGreaterEqual(c.coef,c.varIds,c.rhs, c.name)
              case ConstraintType.LQ => solver.addConstraintLessEqual(c.coef,c.varIds,c.rhs, c.name)
              case ConstraintType.EQ => solver.addConstraintEqual(c.coef,c.varIds,c.rhs, c.name)
        }
        nbC+=1
      }
    }
    
    def getValue(varIndex: Int): Option[Double] = solution.get(varIndex)
    
	def getDual(cons: LPConstraint) : Double = {
		solver.getDual(cons.index)
	}  
	
	def setVarProperties() = {
	  vars  foreach { case (i,x) =>
	    setVarBounds(x)
	    solver.setVarName(x.index, x.getName())
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
		

		solver.startModelBuilding(cons.size,vars.size)
		
		println("Setting variable bounds...")
		setVarProperties() //set the the var bounds correctly
		
		val e = objective.coef.toList
 		val coef : Array[Double] = e.map(_._2).toArray
		val varIds : Array[Int] =  e.map(_._1.index).toArray
		
		println("Creating objective...")
		solver.addObjective(coef, varIds, minimize)
		
		println("Creating constraints...")
		val t0 = System.currentTimeMillis()
		addAllConstraints()
		println("time to add constraints:"+(System.currentTimeMillis()-t0))
				
		//close the model and optimize
		if (autoSolve) solveModel() 
	}
	
	def suchThat (constraints: LinearConstraint*) {
		// add all the constraints
		constraints.foreach(add(_))
		//close the model and optimize
		if (autoSolve) solveModel()
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
     * @return The objective value, 0 if no objective
     */
	def getObjectiveValue() : Double = {
			objective.value match {
				case Some(v) => v
				case None => {
				  0 
				}	
			}
	}

    /**
     * @return The objective value, None if no objective (problem not yet solved or infeasible)
     */	
	def objectiveValue() : Option[Double] = objective.value
	

	def status() : LPStatus.Value = statuss
	
	def release() = solver.release()
	
	def desactivateAutoSolve {autoSolve = false}
	
	
	/**
	 * Check that all the constraints are satisfied
	 */
	def checkConstraints(tol: Double = 10e-6): Boolean = {
	  var violation = false
	  cons  foreach { case (i,c) =>
	    var res = 0.0
	    
	    val ex = c.cstr.linExpr.coef.toArray
	    
	    for ((i,a) <- c.varIds.zip(c.coef)) {
	      val x: AbstractLPVar = vars.get(i) match {
	        case Some(variable) => variable
	        case None => throw new IllegalArgumentException("Variable with index "+i+" not present in lp solver")
	      }
	      res += a * x.getValue
	    }
	    val ok = c.cstr.consType match {
              case ConstraintType.GQ => res+tol >= c.rhs
              case ConstraintType.LQ => res-tol <= c.rhs
              case ConstraintType.EQ => res <= c.rhs+tol && res >= c.rhs-tol
        }
        if (!ok) {
          println("violation of constraint: "+c.name+": "+res+" "+c.cstr.consType+" "+c.rhs)
          violation = true
        }
      }
	  !violation
	}
	
	
} // end class AbstractLPSolver









