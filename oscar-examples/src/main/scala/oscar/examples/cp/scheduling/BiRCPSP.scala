package oscar.examples.cp.scheduling

import oscar.cp.core._
import oscar.cp.modeling._
import scala.io.Source
import oscar.cp.constraints.SweepMaxCumulative
import oscar.cp.scheduling.SchedulingUtils.setTimes

object BiRCPSP extends App {

  // Parse
  // -----
  val lines = Source.fromFile("data/biRCPSP.txt").getLines.drop(2).toArray
  val data = for (line <- lines.tail) yield { line.split("[ \t]") }
  val nTasks = data.head(0).toInt
  val nPrecedences = data.head(1).toInt
  val durations = Array.tabulate(nTasks)(t => data(1+t)(0).toInt)
  val demands = Array.tabulate(nTasks)(t => data(1+t)(1).toInt)
  val precedences = Array.tabulate(nPrecedences)(p => (data(1+nTasks+p)(0).toInt-1, data(1+nTasks+p)(1).toInt-1))
  
  val Tasks = 0 until nTasks
  val horizon = durations.sum
  
  val cp = CPSolver()
  val durationsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t)))
  val startsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsVar(t).min to horizon))
  val demandsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, demands(t)))
  val resourcesVar = Array.fill(nTasks)(CPVarInt(cp, 0))
  
  val makespan = maximum(endsVar)

  // Constraints & Search
  // --------------------

  cp.minimize(makespan) subjectTo {
    // Consistency 
    for (t <- Tasks) {
      cp.add(endsVar(t) == startsVar(t) + durationsVar(t))
    }
    // Precedences
    for ((t1, t2) <- precedences) {
      cp.add(endsVar(t1) <= startsVar(t2))
    }
    // Cumulative
    cp.add(new SweepMaxCumulative(startsVar, endsVar, durationsVar, demandsVar, resourcesVar, CPVarInt(cp, 8), 0))
    
  } exploration {
    //cp.binaryFirstFail(startsVar)
    setTimes(startsVar, durationsVar, endsVar)
  }

  cp.run()
  cp.printStats()
}
