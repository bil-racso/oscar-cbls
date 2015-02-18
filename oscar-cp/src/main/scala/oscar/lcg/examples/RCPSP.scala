package oscar.lcg.examples

import oscar.lcg._
import oscar.lcg.constraints.DecompTT
import oscar.lcg.heuristic.StaticMinHeuristic
import oscar.lcg.heuristic.MinMinHeuristic

object RCPSP extends LCGModel with App {
  
  val instanceFile = "data/rcpsp/j30/j30_1_1.rcp"
  val instance = BLParser.parse(instanceFile)

  // Data
  val nTasks = instance.nTasks
  val Tasks = 0 until nTasks
  val nResources = instance.nResources
  val Resources = 0 until nResources
  
  val scale = 1
  val durations = instance.durations
  val demands = instance.demands.transpose
  val horizon = 43//instance.horizon
  val capa = instance.capacities

  // Decision variables
  val starts = Array.tabulate(nTasks)(t => LCGIntervalVar(0, horizon - durations(t), s"start_$t"))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  
  // Precedences
  for ((t1, t2) <- instance.precedences) add(ends(t1) <= starts(t2))
  
  // Resources
  for (r <- Resources) {
    add(new DecompTT(starts, durations, demands(r), capa(r), horizon))
  }

  // Search heuristic
  val heuristic = new StaticMinHeuristic(starts)
  
  onSolution {
    println("\nSOLUTION\n" + starts.map(v => v.name + " = " + v.min).mkString("\n"))
    println("makespan: " + ends.map(_.max).max)
    println
    for (r <- Resources) println(s"Valid on $r: " + SolutionChecker.check(starts.map(_.min), durations, demands(r), capa(r), horizon))
    val valid = instance.precedences.forall { case (t1, t2) => ends(t1).min <= starts(t2).min }
    println("check precedence: " + valid)
  }
  
  val t0 = System.currentTimeMillis()
  
  solve(heuristic)
  
  println()
  println("time (ms)  : " + (System.currentTimeMillis() - t0))
  println("nConflicts : " + lcgSolver.search.nConflicts)
  println("nNodes     : " + lcgSolver.search.nNodes)
  println("nLearnt    : " + lcgSolver.cdclStore.nLeanrt)
  println("nSolutions : " + lcgSolver.search.nSolutions)
}