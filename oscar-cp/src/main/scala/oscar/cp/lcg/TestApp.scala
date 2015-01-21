package oscar.cp.lcg

import oscar.cp.lcg._

object TestApp extends LCGModel with App {

  def propagate(): Unit = {
    println
    println("propagate results : " + lcgStore.propagate())
    println(x.name + "[" + x.min + ", " + x.max + "]")
    println(x)
    while (lcgStore.isInconsistent) {
      println("pop state")
      cpSolver.pop()
    }
  }

  cpSolver.pushState()

  val x = LCGIntervalVar(5, 25, "x")

  propagate() 
  propagate()
  
  lcgStore.enqueue(x.minGeq(10), null)

  propagate()
  propagate()

  lcgStore.addExplanationClause(Array(-x.minGeq(10)))

  propagate()
  propagate()

  lcgStore.enqueue(x.minGeq(10), null)

  propagate()
  propagate()
  
  cpSolver.pushState()
  
  lcgStore.enqueue(x.minGeq(15), null)
  lcgStore.enqueue(x.maxLeq(20), null)
  
  cpSolver.pushState()
  cpSolver.pushState()
  cpSolver.pushState()
  cpSolver.pushState()

  propagate()
  propagate()
  
  lcgStore.addExplanationClause(Array(-x.minGeq(10)))
  
  propagate()
  propagate()
}