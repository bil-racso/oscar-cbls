package oscar.lcg.examples

import oscar.lcg._
import oscar.lcg.constraints.DecompCumulative

object RCPSP extends App {

  println("name\ttime\tfails\tbest\topt")
  for (j <- 1 to 5; i <- 1 to 10) {

    new LCGModel {

      val instanceFile = s"j30_${j}_${i}"
      val instance = BLParser.parse("data/rcpsp/j30/" + instanceFile + ".rcp")

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
      for (r <- Resources) {
        add(cumulative(starts, durations, demands(r), capa(r)))
      }

      // Search heuristic
      val heuristic = minValue(starts)

      onSolution {
        //val validOnResources = Resources.forall(r => SolutionChecker.check(starts.map(_.min), durations, demands(r), capa(r), horizon))
        //val validPrecedences = instance.precedences.forall { case (t1, t2) => ends(t1).min <= starts(t2).min }
        //if (!validPrecedences && !validOnResources) sys.error("PROBLEM")
        best = ends.map(_.max).max
      }

      val t0 = System.currentTimeMillis()

      var best = horizon
      var optimum = false
      var stop = false
      while (!optimum && !stop) {
        val success = ends.forall(end => add(end.lowerEqual(best)))
        if (!success) {
          optimum = true
        } else {
          val out = lcgSolver.solve(heuristic, System.currentTimeMillis() - t0 >= 10000, resetStatistics = false)
          if (out == False) optimum = true
          else if (out == Unassigned) stop = true
          else best -= 1 
        }
      }

      best +=1
      val time = System.currentTimeMillis() - t0
      val conflicts = lcgSolver.search.nConflicts
      
      println(instanceFile + "\t" + time + "\t" + conflicts + "\t" + best + "\t" + optimum)
    }
  }
}