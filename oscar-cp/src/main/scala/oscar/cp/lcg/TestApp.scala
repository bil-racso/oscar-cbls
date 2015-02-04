package oscar.cp.lcg

import oscar.cp._
import oscar.cp.lcg._
import oscar.cp.lcg.core.LCGSolver
import oscar.cp.lcg.constraints.LCGConstraint
import oscar.cp.lcg.core.Literal

object TestApp extends LCGModel with App {

  val variables = Array.tabulate(3)(i => LCGIntervalVar(0, 4, "Var_" + i))

  class DumpConstraint(lcgSolver: LCGSolver, variable: LCGIntervalVar) extends LCGConstraint(lcgSolver, variable.store, "") {
    override def register(): Unit = variable.callWhenBoundsChange(this)
    final override def explain(): Unit = {
      if (variable.min == 2 && variable.max == 2) {
        val explanation = new Array[Literal](2)
        explanation(0) = -variable.maxLeq(2)
        explanation(1) = -variable.minGeq(2)
        lcgStore.addExplanation(explanation)
      }
    }
  }

  add(new DumpConstraint(lcgSolver, variables(1)))

  search {
    val variable = variables.find(v => !v.isAssigned)
    if (variable.isEmpty) noAlternative
    else {
      val v = variable.get
      val value = v.min
      val literal = v.maxLeq(value)
      branch {
        // Assign
        //println
        //println(variables(0))
        //println(variables(1))
        //println(variables(2))
        //println("assign " + v.name + " to " + value)
        lcgSolver.lcgStore.newDecisionLevel()
        cpSolver.doAndPropagate {
          lcgSolver.lcgStore.enqueue(literal, null)
        }
        //println(variables(0))
        //println(variables(1))
        //println(variables(2))
      } {
        // Remove
        //println
        //println(variables(0))
        //println(variables(1))
        //println(variables(2))
        //println("remove " + value + " from " + v.name)
        lcgSolver.lcgStore.newDecisionLevel()
        cpSolver.doAndPropagate {
          lcgSolver.lcgStore.enqueue(-literal, null)
        }
        //println(variables(0))
        //println(variables(1))
        //println(variables(2))
      }
    }
  }

  onSolution()//println("SOLUTION FOUND !"))

  println(start())
}