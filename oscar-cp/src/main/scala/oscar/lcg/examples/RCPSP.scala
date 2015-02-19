package oscar.lcg.examples

import oscar.lcg._

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
  val horizon = instance.horizon
  val capa = instance.capacities

  // Decision variables
  val starts = Array.tabulate(nTasks)(t => LCGIntervalVar(0, horizon - durations(t), s"start_$t"))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durations(t))
  
  // Precedences
  for ((t1, t2) <- instance.precedences) add(ends(t1) <= starts(t2))
  
  // Resources
  for (r <- Resources) add(cumulative(starts, durations, demands(r), capa(r)))

  // Search heuristic
  val heuristic = static(starts)
  
  onSolution {
    val validOnResources = Resources.forall(r => SolutionChecker.check(starts.map(_.min), durations, demands(r), capa(r), horizon))
    val validPrecedences = instance.precedences.forall { case (t1, t2) => ends(t1).min <= starts(t2).min }
    println("makespan: " + ends.map(_.max).max + ", resources: " + validOnResources + ", precedences: " + validPrecedences)    
    //Resources.foreach(r => println("resource " + r + ": " + SolutionChecker.check(starts.map(_.min), durations, demands(r), capa(r), horizon)))
    best = ends.map(_.max).max
  }
  
  val t0 = System.currentTimeMillis()
  
  var best = horizon
  var optimum = false
  while (!optimum) {
    val success = ends.forall(end => add(end.lowerEqual(best)))
    if (!success) {
      optimum = true
      println("here")
    }
    else if (lcgSolver.solve(heuristic, false, resetStatistics = false) == False) optimum = true
    else best -= 1
  }
  
  println("best " + (best + 1))
  
  println()
  println("time (ms)  : " + (System.currentTimeMillis() - t0))
  println("nConflicts : " + lcgSolver.search.nConflicts)
  println("nNodes     : " + lcgSolver.search.nNodes)
  println("nLearnt    : " + lcgSolver.cdclStore.nLeanrt)
  println("nSolutions : " + lcgSolver.search.nSolutions)
}