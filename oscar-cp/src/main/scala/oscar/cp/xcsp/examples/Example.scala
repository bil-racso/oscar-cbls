package oscar.cp.xcsp.examples
/*
import oscar.cp.modeling._
import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import java.io.File
import oscar.cp.xcsp.modeling.DefaultConstraints
import oscar.cp.xcsp.XCSPSolver

object Example extends App {
 
  val xcspSolver = new XCSPSolver with DefaultConstraints {
    override def allDifferent(vars: Iterable[CPIntVar]) : Constraint = oscar.cp.modeling.allDifferent(vars)
  }
  
  val (cp, vars) = xcspSolver.model(new File(args(0)))
 
  cp.onSolution{
    vars.toSeq.sortBy(v=>v.name)foreach{v=>print(v.name + "=" + v.value+ " ")}
    println
  }
  cp.search(binaryFirstFail(vars.toSeq))
  println(cp.start())
 
}*/