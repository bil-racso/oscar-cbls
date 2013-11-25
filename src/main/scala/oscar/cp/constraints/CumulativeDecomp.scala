package oscar.cp.constraints

import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPSolver

class CumulativeDecomp(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int) extends Constraint(starts.head.store, "Cumulative decomposition") {

  private val maxEndDate = maxValue(ends)
  private val minEndDate = minValue(starts)
  private val Horizon = minEndDate to maxEndDate

  private val nTasks = starts.size
  private val Tasks = 0 until nTasks

  private def overlapingTasks(t: Int): IndexedSeq[Int] = {
    Tasks.filter(task => starts(task).min <= t && ends(task).max > t)
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    for (t <- Horizon) {
      val tasks = overlapingTasks(t)
      val overlapingVars = tasks.map(task => ((starts(task) <== t) and (ends(task) >>= t) and (resources(task) === id)) * demands(task))
      val minDemand = minSum(overlapingVars)
      val maxDemand = maxSum(overlapingVars)
      val totDemand = CPVarInt(s, minDemand to maxDemand)
      if (s.post(new Sum(overlapingVars.toArray, totDemand)) == Failure) return Failure
      if (s.post(totDemand <= capacity.max) == Failure) return Failure
    }
    Suspend
  }

  private def maxValue(vars: Seq[CPVarInt]): Int = {
    var max = vars.head.max
    vars.tail.foreach(x => max = math.max(max, x.max))
    max
  }

  private def minValue(vars: Seq[CPVarInt]): Int = {
    var min = vars.head.min
    vars.tail.foreach(x => min = math.min(min, x.min))
    min
  }

  private def minSum(vars: Seq[CPVarInt]): Int = {
    var sum = 0
    vars.foreach(sum += _.min)
    sum
  }

  private def maxSum(vars: Seq[CPVarInt]): Int = {
    var sum = 0
    vars.foreach(sum += _.max)
    sum
  }
}

object CumulativeDecomp extends App {

  val cp = CPSolver()
  val horizon = 5
  val capacity = CPVarInt(cp, 2)
  val durations = Array(2, 2, 2)
  val demands = Array(1, 1, 2)
  val nTasks = durations.size
  val Tasks = 0 until nTasks

  val durationsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t)))
  val startsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsVar(t).min to horizon))
  val demandsVar = Array.tabulate(nTasks)(t => CPVarInt(cp, demands(t)))
  val resourcesVar = Array.fill(nTasks)(CPVarInt(cp, 0))

  cp.solve subjectTo {
    for (t <- Tasks) {
      cp.add(endsVar(t) == startsVar(t) + durationsVar(t))
    }
    cp.add(new CumulativeDecomp(startsVar, endsVar, durationsVar, demandsVar, resourcesVar, capacity, 0))
  }

  val allSols = Set(
    List(0, 0, 2),
    List(0, 0, 3),
    List(1, 0, 3),
    List(0, 1, 3),
    List(1, 1, 3),
    List(3, 3, 1),
    List(2, 2, 0),
    List(3, 2, 0),
    List(2, 3, 0),
    List(3, 3, 0))

  var nSol = 0
  cp.exploration {
    cp.binaryFirstFail(startsVar)
    nSol += 1
    val sol = startsVar.map(_.value).toList
    println(sol.mkString(" "))
    assert(allSols contains sol)
  }
  
  cp.run()

  assert(nSol == 10)
}