package oscar

import oscar.linprog.modeling._
import oscar.algebra._

package object linprog {
  
  val rand = new scala.util.Random(12)
  
  
  object LPSolverLib extends Enumeration {
    val lp_solve = Value("lp_solve")
    val cplex = Value("cplex")
    val glpk = Value("glpk")
    val gurobi = Value("gurobi")
  }
  
  // solvers used for test
  val solvers = List(LPSolverLib.lp_solve, LPSolverLib.glpk,LPSolverLib.cplex)
  
  /*
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
	*/
}