package oscar.cp.lcg

import oscar.cp._
import oscar.cp.lcg._
import oscar.cp.lcg.constraints.DecompChecker

object TestScheduling extends LCGModel with App {

  // Data
  val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2), (20, 2), (80, 1))
  val durations = instance.map(_._1)
  val demands = instance.map(_._2)
  val horizon = durations.sum
  val nTasks = durations.length
  val capa = 4

  // Decision variables
  val starts = Array.tabulate(nTasks)(t => LCGIntervalVar(0, horizon - durations(t), "Task_" + t))

  // LCG Constraints
  for (time <- 0 to horizon) cpSolver.add(new DecompChecker(lcgSolver, starts, durations, demands, capa, time))

  search {
    val variable = starts.find(v => !v.isAssigned)
    if (variable.isEmpty) noAlternative
    else {
      val v = variable.get
      val value = v.min
      branch {
        lcgSolver.lcgStore.newDecisionLevel()
        // Assign
        println("assign " + v.name)
        cpSolver.doAndPropagate {
          lcgSolver.lcgStore.enqueue(v.minGeq(value), null)
          lcgSolver.lcgStore.enqueue(v.maxLeq(value), null)
          lcgSolver.propagate()
        }
      } {
        lcgSolver.lcgStore.newDecisionLevel()
        // Remove
        cpSolver.doAndPropagate {
          lcgSolver.lcgStore.enqueue(-v.minGeq(value), null)
          lcgSolver.propagate()
        }
      }
    }
  }

  onSolution(println("SOLUTION FOUND !"))

  start(nSols = 1)

  // 

}