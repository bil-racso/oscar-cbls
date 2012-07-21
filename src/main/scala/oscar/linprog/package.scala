package oscar

package object linprog {
  
  object LPSolverLib extends Enumeration {
    val lp_solve = Value("lp_solve")
    val cplex = Value("cplex")
    val glpk = Value("glpk")
    val gurobi = Value("gurobi")
  }
  
  // solvers used for test
  val solvers = List(LPSolverLib.lp_solve, LPSolverLib.glpk,LPSolverLib.cplex)

}