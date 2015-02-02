package oscar.cp.lcg

import oscar.cp.lcg._

object TestApp extends LCGModel with App {

  def propagate(): Unit = {
    println
    println("propagate results : " + lcgSolver.propagate())
    println(x.name + "[" + x.min + ", " + x.max + "]")
    println(x)
    println(lcgSolver.decisionLevel)
    while (lcgSolver.isInconsistent) {
      println("pop state sat level before : " + lcgSolver.decisionLevel)
      cpSolver.pop()
      println("pop state sat level after : " + lcgSolver.decisionLevel)
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

  println("\n start fix point at sat level " + lcgSolver.decisionLevel +" and cp level 5 \n")
  propagate()
  propagate()
  
  lcgSolver.addExplanationClause(Array(-x.minGeq(10)))
  
  propagate()
  propagate()
}