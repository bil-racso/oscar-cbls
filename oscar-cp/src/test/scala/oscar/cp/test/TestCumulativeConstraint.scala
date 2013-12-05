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
  
  case class SchedulingInstance(val durations: Array[Int], val demands: Array[Int], val resources: Array[Int], val capacity: Array[Int], val horizon: Int)
  
  class CPSched(instance: SchedulingInstance) extends CPSolver {
    silent = true
    val nTasks = instance.demands.size
    val Tasks = 0 until nTasks
    val nResources = instance.capacity.size
    val Resources = 0 until nResources
    val durations = Array.tabulate(nTasks)(t => CPVarInt(this, instance.durations(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(this, 0 to instance.horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(this, durations(t).min to instance.horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(this, instance.demands(t)))
    val resources = Array.tabulate(nTasks)(t => CPVarInt(this, instance.resources(t)))
    Tasks.foreach(t => post(ends(t) == starts(t) + durations(t)))
  }
  
  def cumulative(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int): Constraint
  
  def generateRandomSchedulingProblem(nTasks: Int): SchedulingInstance = {
    val rand = new scala.util.Random()
    val durations = Array.fill(nTasks)(rand.nextInt(4)) 
    val demands = Array.fill(nTasks)(rand.nextInt(4))
    val capacity = Array(rand.nextInt(4) + 1)
    val resources = Array.fill(nTasks)(0)
    val horizon = rand.nextInt(durations.sum+1-durations.max) + durations.max
    SchedulingInstance(durations, demands, resources, capacity, horizon)
  }
  
  def solveAll(cp: CPSched, capacity: Array[Int], decomp: Boolean): Set[Sol] = {
    cp.solve
    cp.subjectTo {
      if (!decomp) cp.Resources.foreach(r => cp.post(cumulative(cp.starts, cp.ends, cp.durations, cp.demands, cp.resources, CPVarInt(cp, capacity(r)), r)))
      else cp.Resources.foreach(r => cp.post(new CumulativeDecomp(cp.starts, cp.ends, cp.durations, cp.demands, cp.resources, CPVarInt(cp, capacity(r)), r)))
    }
    var sols: List[Sol] = List()
    cp.exploration {
      cp.binaryFirstFail(cp.starts)
      cp.binaryFirstFail(cp.durations)
      cp.binaryFirstFail(cp.resources)
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
  
  test("test solveAll " + cumulativeName) {
    (1 to nTests).forall(i => {
      print("test " + cumulativeName + " instance " + i + ": ")
      val instance = generateRandomSchedulingProblem(5)
      val cpDecomp = new CPSched(instance)
      val cpCumul = new CPSched(instance)
      val allSolsDecomp = solveAll(cpDecomp, instance.capacity, true)
      val allSolsCumul = solveAll(cpCumul, instance.capacity, false)
      if (compare(allSolsDecomp, allSolsCumul)) {
        println("success " + allSolsDecomp.size + " " + allSolsCumul.size)
        true
      }
      else {
        println("failed !")    
        println("expected number of solutions: " + allSolsDecomp.size)
        println("number of solutions: " + allSolsCumul.size)
        println("INSTANCE")
        println("durations: " + instance.durations.mkString(", "))
        println("demands: " + instance.demands.mkString(", "))
        println("resources: " + instance.resources.mkString(", "))
        println("capacity: " + instance.capacity.mkString(", "))
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
  
  test("myTest") {
    
  }
}

class TestEnergeticReasoning extends TestCumulativeConstraint("Energetic") {
  override def cumulative(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int): Constraint = {
    new EnergeticReasoning(starts, ends, durations, demands, resources, capacity, id: Int)
  }
}
