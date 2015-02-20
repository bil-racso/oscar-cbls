package oscar.lcg.benchmarks

import scala.util.Random
import oscar.lcg.testUtils._

/** @author Renaud Hartert ren.hartert@gmail.com */

class RCPSPSuite extends TestSuite {

  // Test parameters
  final val nTasks = 6
  final val seed = 0
  final val minDuration = 0
  final val minDemand = 0
  final val nTests = 100
  
  // Random number generator
  private val rng = new Random(seed)

  // Generates a random RCPSP instance.
  private def randomInstance(nTasks: Int, minDuration: Int, minDemand: Int, rng: Random): RCPSPInstance = {
    val durations = Array.fill(nTasks)(rng.nextInt(4) + minDuration)
    val demands = Array.fill(nTasks)(rng.nextInt(4) + minDemand)
    val capacity = rng.nextInt(4) + 1 + minDemand
    val horizon = rng.nextInt(durations.sum + 1 - durations.max) + durations.max
    RCPSPInstance(durations, demands, capacity, horizon)
  }

  private def compareSolutions(instance: RCPSPInstance): Boolean = {
    val lcgSolver = new RCPSPSolverLCG(instance)
    val cpSolver = new RCPSPSolverCP(instance)
    var optimum = false
    var best = instance.horizon
    while (!optimum) {
      val lcgSolution = lcgSolver.solve(best)
      val cpSolution = cpSolver.solve(best)
      if (lcgSolution == null && cpSolution != null) return false
      else if (lcgSolution != null && cpSolution == null) return false
      else if (lcgSolution == null && cpSolution == null) optimum = true
      else {
        best -= 1
        val same = (0 until lcgSolution.length).forall(t => cpSolution(t) == lcgSolution(t))
        if (!same) return false
      }
    }
    true
  }

  test("static search should yield the same solutions in LCG and CP.") {
    (0 until nTests).foreach(t => {
      val instance = randomInstance(nTasks, minDuration, minDemand, rng)
      val same = compareSolutions(instance)
      assert(same, s"Different solution !\n$instance")
    })
  }
}

case class RCPSPInstance(durations: Array[Int], demands: Array[Int], capacity: Int, horizon: Int) {
  override def toString: String = {
    val buffer = new StringBuffer()
    buffer.append("Instance:\n")
    buffer.append(s"val durations = Array(${durations.mkString(", ")})\n")
    buffer.append(s"val demands = Array(${demands.mkString(", ")})\n")
    buffer.append(s"val capacity = $capacity\n")
    buffer.append(s"val horizon = $horizon")
    buffer.toString
  }
}

abstract class RCPSPSolver { def solve(horizon: Int): Array[Int] }

class RCPSPSolverLCG(instance: RCPSPInstance) extends RCPSPSolver {
  import oscar.lcg._
  // Data
  val nTasks = instance.durations.length
  val durations = instance.durations
  val demands = instance.demands
  val capacity = instance.capacity
  val horizon = instance.horizon
  implicit val solver = new LCGSolver()
  val starts = Array.tabulate(nTasks)(task => LCGIntervalVar(0, horizon - durations(task)))
  val ends = Array.tabulate(nTasks)(task => starts(task) + durations(task))
  solver.add(cumulative(starts, durations, demands, capacity))
  val heuristic = static(starts)
  var solution: Array[Int] = null
  solver.onSolution(solution = starts.map(_.min))

  final override def solve(horizon: Int): Array[Int] = {
    solution = null
    val success = ends.forall(t => solver.add(t.lowerEqual(horizon)))
    if (success) solver.solve(heuristic, false)
    solution
  }
}

class RCPSPSolverCP(instance: RCPSPInstance) extends RCPSPSolver {
  import oscar.cp._
  // Data
  val nTasks = instance.durations.length
  val durations = instance.durations
  val demands = instance.demands
  val capacity = instance.capacity
  val horizon = instance.horizon
  implicit val solver = CPSolver()
  val startsVar = Array.tabulate(nTasks)(task => CPIntVar(0, horizon - durations(task)))
  val endsVar = Array.tabulate(nTasks)(task => startsVar(task) + durations(task))
  val durationsVar = durations.map(CPIntVar(_))
  val demandsVar = demands.map(CPIntVar(_))
  val capacityVar = CPIntVar(capacity)
  solver.post(maxCumulativeResource(startsVar, durationsVar, endsVar, demandsVar, capacityVar))
  search(binaryStatic(startsVar))
  var solution: Array[Int] = null
  onSolution(solution = startsVar.map(_.min))

  final override def solve(horizon: Int): Array[Int] = {
    solution = null
    endsVar.foreach(x => solver.post(x <= horizon))
    solver.start(nSols = 1)
    solution
  }
}