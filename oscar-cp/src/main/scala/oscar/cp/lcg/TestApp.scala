package oscar.cp.lcg

import oscar.cp._
import oscar.cp.lcg._

object TestApp extends LCGModel with App {
  
  val variables = Array.tabulate(3)(i => LCGIntervalVar(0, 2, "Var_" + i))

  search {
    val variable = variables.find(v => !v.isAssigned)
    if (variable.isEmpty) noAlternative
    else {
      val v = variable.get
      val value = v.min
      branch {
        // Assign
        println("assign " + value + " to " + v.name)
        lcgSolver.lcgStore.newDecisionLevel()
        cpSolver.doAndPropagate {
          lcgSolver.lcgStore.enqueue(v.minGeq(value), null)
          lcgSolver.lcgStore.enqueue(v.maxLeq(value), null)
        }
      } {
        // Remove
        println("remove " + " value " + " to " + v.name)
        lcgSolver.lcgStore.newDecisionLevel()
        cpSolver.doAndPropagate {
          lcgSolver.lcgStore.enqueue(-v.minGeq(value), null)
        }
      }
    }
  }

  onSolution(println("SOLUTION FOUND !"))

  start()
}