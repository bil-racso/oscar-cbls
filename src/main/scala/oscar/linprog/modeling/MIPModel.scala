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


import oscar.linprog._


/**
 * @author Pierre Schaus pschaus@gmail.com  
 */
class MIPVar(mip: MIPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity) extends AbstractLPVar(mip,name,lbound,ubound,false) {

	  	def this(mip: MIPSolver, name: String, unbounded: Boolean) = {
	      this(mip,name)
	      this.unbounded = unbounded 
	    }
	  
		/**
		 * Defines an Integer variable in the MIP solver with the specified integer range domain 
		 */
		def this(mip : MIPSolver, name : String,  domain : Range) = {
			this(mip, name, domain.min, domain.max)
			this.integer = true
		}

		/**
		 * Set the variable as an integer one
		 */
		def setInteger() {
			this.integer = true
		}		
		
}
	
object MIPVar { 
	  def apply(mip: MIPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity): MIPVar = new MIPVar(mip,name,lbound,ubound) 
	  def apply(mip : MIPSolver, name : String,  domain : Range): MIPVar =  new MIPVar(mip,name,domain)
}

class MIPSolver(solverLib: LPSolverLib.Value = LPSolverLib.lp_solve) extends AbstractLPSolver() {

    val solver = solverLib match {
      case LPSolverLib.lp_solve => new LPSolve()
      case LPSolverLib.glpk => new GlpkMIP()
      case LPSolverLib.cplex => new CplexLP()
      case LPSolverLib.gurobi => new GurobiLP()
      case _ => new LPSolve()
    }

    override def setVarProperties() = {
      super.setVarProperties();
      for (x <- vars; if (x._2.isInteger)) {
        solver.setInteger(x._2.index)
      }
    }

}
	
object MIPSolver { 
	 def apply(solverLib: LPSolverLib.Value = LPSolverLib.lp_solve): MIPSolver = new MIPSolver(solverLib) 
}
