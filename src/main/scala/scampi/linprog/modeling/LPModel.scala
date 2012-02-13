/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.modeling


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
  	
	class LPVar(lp: LPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity) extends AbstractLPVar(lp,name,lbound,ubound,false) {

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

