package oscar.examples.cp.scheduling

import oscar.cp.core._
import oscar.cp.modeling._
import scala.io.Source

object BiRCPSP extends CPModel with App {

  // Parse
  // -----
  val lines = Source.fromFile("data/biRCPSP.txt").getLines.drop(2).toArray
  val data = for (line <- lines.tail) yield { line.split("[ \t]") }
  val nTasks = data.head(0).toInt
  val nPrecedences = data.head(1).toInt
  val durations = Array.tabulate(nTasks)(t => data(1 + t)(0).toInt)
  val demands = Array.tabulate(nTasks)(t => data(1 + t)(1).toInt)
  val precedences = Array.tabulate(nPrecedences)(p => (data(1 + nTasks + p)(0).toInt - 1, data(1 + nTasks + p)(1).toInt - 1))

  val Tasks = 0 until nTasks
  val horizon = durations.sum

  val durationsVar = Array.tabulate(nTasks)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nTasks)(t => CPIntVar(0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nTasks)(t => CPIntVar(durationsVar(t).min to horizon))
  val demandsVar = Array.tabulate(nTasks)(t => CPIntVar(demands(t)))
  val resourcesVar = Array.fill(nTasks)(CPIntVar(0))

  val makespan = maximum(endsVar)

  // Constraints & Search
  // --------------------

  // Consistency 
  for (t <- Tasks) {
    add(endsVar(t) == startsVar(t) + durationsVar(t))
  }
  
  // Precedences
  for ((t1, t2) <- precedences) {
    add(endsVar(t1) <= startsVar(t2))
  }
  
  // Cumulative
  add(maxCumulativeResource(startsVar, durationsVar, endsVar, demandsVar, resourcesVar, CPIntVar(8), 0))

  minimize(makespan) search {
    setTimes(startsVar, durationsVar, endsVar)
  }

  println(start())
}
