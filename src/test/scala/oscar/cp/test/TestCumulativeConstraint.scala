package oscar.cp.test

import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.constraints.SweepMaxCumulative
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.modeling._
import oscar.cp.core.CPVarInt
import oscar.cp.constraints.CumulativeDecomp
import oscar.cp.constraints.EnergeticReasoning

abstract class TestCumulativeConstraint(cumulativeName: String, nTests: Int = 100) extends FunSuite with ShouldMatchers { 
  
  type Sol = List[Int]
  
  case class SchedulingInstance(val durations: Array[Int], val demands: Array[Int], val resources: Array[Int], val horizon: Int, val capacity: Int)
  
  class CPSched(instance: SchedulingInstance) extends CPSolver {
    silent = true
    val nTasks = instance.demands.size
    val Tasks = 0 until nTasks
    val durations = Array.tabulate(nTasks)(t => CPVarInt(this, instance.durations(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(this, 0 to instance.horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(this, durations(t).min to instance.horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(this, instance.demands(t)))
    val resources = Array.fill(nTasks)(CPVarInt(this, 0))
    Tasks.foreach(t => this.post(ends(t) == starts(t) + durations(t)))
  }
  
  def cumulative(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int): Constraint
  
  def generateRandomSchedulingProblem(nTasks: Int): SchedulingInstance = {
    val rand = new scala.util.Random()
    val durations = Array.fill(nTasks)(rand.nextInt(3)) 
    val demands = Array.fill(nTasks)(rand.nextInt(3))
    val capacity = rand.nextInt(3) + 2
    val resources = Array.fill(nTasks)(1)
    val horizon = rand.nextInt(3) + durations.sum
    SchedulingInstance(durations, demands, resources, horizon, capacity)
  }
  
  def testCumulative(nTests: Int): Boolean = {
    (1 to nTests).forall(_ => {
      val instance = generateRandomSchedulingProblem(5)
      val cpDecomp = new CPSched(instance)
      val cpCumul = new CPSched(instance)
      val allSolsDecomp = solveAll(cpDecomp, instance.capacity, true)
      val allSolsCumul = solveAll(cpCumul, instance.capacity, false)
      if (allSolsDecomp.size != allSolsCumul.size) false
      else allSolsCumul.forall(sol => allSolsCumul contains sol)
    })
  }
  
  def solveAll(cp: CPSched, capacity: Int, decomp: Boolean): Set[Sol] = {
    cp.solve
    val cumul = if (!decomp) cumulative(cp.starts, cp.ends, cp.durations, cp.demands, cp.resources, CPVarInt(cp, capacity), 1)
    else new CumulativeDecomp(cp.starts, cp.ends, cp.durations, cp.demands, cp.resources, CPVarInt(cp, capacity), 1)
    cp.subjectTo {
      cp.add(cumul)
    }
    var sols: List[Sol] = List()
    cp.exploration {
      cp.binaryFirstFail(cp.starts)
      val sol = cp.starts.map(_.value).toList
      sols = sol :: sols
    }
    cp.run()
    sols.toSet
  }
  
  def compare(sols1: Set[Sol], sols2: Set[Sol]): Boolean = {
    if (sols1.size != sols2.size) false
    else sols1.forall(s => sols2 contains s)
  }
  
  test("test " + cumulativeName) {
    (1 to nTests).forall(i => {
      print("test " + cumulativeName + " instance " + i + ": ")
      val instance = generateRandomSchedulingProblem(5)
      val cpDecomp = new CPSched(instance)
      val cpCumul = new CPSched(instance)
      val allSolsDecomp = solveAll(cpDecomp, instance.capacity, true)
      val allSolsCumul = solveAll(cpCumul, instance.capacity, false)
      if (compare(allSolsDecomp, allSolsCumul)) {
        println("success")
        true
      }
      else {
        println("failed !")
        println("durations: " + instance.durations.mkString(", "))
        println("demands: " + instance.demands.mkString(", "))
        println("resources: " + instance.resources.mkString(", "))
        println("capacity: " + instance.capacity)
        println("horizon: " + instance.horizon)
        false
      }
    }) should be(true)
  } 
}

class TestSweepMaxCumulative2 extends TestCumulativeConstraint("SweepMaxCumulative") {
  override def cumulative(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int): Constraint = {
    new SweepMaxCumulative(starts, ends, durations, demands, resources, capacity, id: Int)
  }
}

class TestEnergeticReasoning extends TestCumulativeConstraint("EnergeticReasoning") {
  override def cumulative(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int): Constraint = {
    new EnergeticReasoning(starts, ends, durations, demands, resources, capacity, id: Int)
  }
}