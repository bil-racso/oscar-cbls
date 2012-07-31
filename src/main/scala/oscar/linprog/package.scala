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
  


}
