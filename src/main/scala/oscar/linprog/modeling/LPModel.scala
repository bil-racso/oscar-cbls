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


/**
 * Abstract class that must be extended to define a new LP solver
 * @author Pierre Schaus
 */
abstract class LP extends AbstractLP {

	/**
	 * Get the reduced cost corresponding to the column/variable
	 */
	def getReducedCost(colId : Int) : Double
}

/**
 * Trait to extend by the class defining your LP model 
 * @author Pierre Schaus
 */
trait LPModel extends AbstractLPModel {
  	
  case class LPVar(lp: LPSolver, name_ : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity) extends AbstractLPVar(lp,name_,lbound,ubound,false) {

	    def this(lp: LPSolver, name: String, unbounded: Boolean) = {
	      this(lp,name,if (unbounded) Double.PositiveInfinity else 0.0,Double.PositiveInfinity)
	      this.unbounded = unbounded 
	    }
	    
		def getReducedCost() : Double = lp.getReducedCost(index)
	}
	
	class LPSolver(solverLib: LPSolverLib.Value = LPSolverLib.lp_solve) extends AbstractLPSolver() {
	  
		val solver = solverLib match {
		  case LPSolverLib.lp_solve => new LPSolve()
		  case LPSolverLib.glpk => new GlpkLP()
		  case _  => new LPSolve()		
		}

		def getReducedCost(varId : Int) : Double = solver.getReducedCost(varId)

		def addColumn(objCoef : Double, constraints : IndexedSeq[LPConstraint], lhsConstraintCoefs : Array[Double]) : LPVar = {
		    val colVar = new LPVar(this,"column var")
			objective += (objCoef * colVar)
			solver.addColumn(objCoef, constraints.map(_.index).toArray, lhsConstraintCoefs)
			solveModel()
			colVar 
		}

	}
	
	object LPSolver {
	  def apply(solverLib: LPSolverLib.Value = LPSolverLib.lp_solve): LPSolver = new LPSolver(solverLib)
	}
}

