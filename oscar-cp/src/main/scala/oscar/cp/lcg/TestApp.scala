package oscar.cp.lcg

import oscar.cp.lcg._

object TestApp extends LCGModel with App {

  def propagate(): Unit = {
    println
    println("propagate results : " + lcgSolver.propagate())
    println(x.name + "[" + x.min + ", " + x.max + "]")
    println(x)
    while (lcgSolver.isInconsistent) {
      println("pop state")
      cpSolver.pop()
    }
  }

  cpSolver.pushState()

  val x = LCGIntervalVar(5, 25, "x")

  propagate() 
  propagate()
  
  lcgSolver.enqueue(x.minGeq(10), null)

  propagate()
  propagate()

  lcgSolver.addExplanationClause(Array(-x.minGeq(10)))

  propagate()
  propagate()

  lcgSolver.enqueue(x.minGeq(10), null)

  propagate()
  propagate()
  
  cpSolver.pushState()
  
  lcgSolver.enqueue(x.minGeq(15), null)
  lcgSolver.enqueue(x.maxLeq(20), null)
  
  cpSolver.pushState()
  cpSolver.pushState()
  cpSolver.pushState()
  cpSolver.pushState()

  propagate()
  propagate()
  
  lcgSolver.addExplanationClause(Array(-x.minGeq(10)))
  
  propagate()
  propagate()
}