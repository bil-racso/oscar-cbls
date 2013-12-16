package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._

// intention is to optimise a progressively larger set of problem variables
// reusing the same CPSolver
// eventually restricting values in later runs based on optimal soln for earlier runs

object Test extends App {
  val cp = CPSolver()
  val x1 = CPVarInt(cp, 1 to 2)
  val x2 = CPVarInt(cp, 1 to 2)
  val x3 = CPVarInt(cp, 1 to 2)

  val allVars = Array(x1, x2, x3)
  val soln = Array(-1, -1, -1)

  for (i <- 1 to 3) {
    // optimise using just the first i variables
    println("run " + i)
    val optVars = Array.tabulate(i)(allVars(_))

    // I tried "deactivating" cut constraints here but that didn't seem to help

    cp.minimize(sum(optVars)) subjectTo {

      // maybe constrain some variables here based on previous best solutions

    }

    cp.exploration {

      println("x1 domain size " + x1.size)
      println("x2 domain size " + x2.size)
      println("x3 domain size " + x3.size)

      cp.binaryFirstFail(optVars, _.min)

      for (j <- 0 until i) {
        soln(j) = optVars(j).value
      }

      println("soln " + soln.mkString(" "))

      // fail here keeps the variable domains unconstrained
      // but prevents objective function bounds being applied
      //cp.fail

    } run ()

    println("best soln " + soln.mkString(" "))

  }
}