package oscar.cp.test

import oscar.cp.core.Constraint
import oscar.cp.constraints.SweepMaxCumulative
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.modeling._
import oscar.cp.core.CPIntVar
import oscar.cp.constraints.CumulativeDecomp
import oscar.cp.search.BinaryStaticOrderBranching
import oscar.cp.constraints.EnergeticReasoning

abstract class TestCumulativeConstraint(val cumulativeName: String, val nTests: Int = 100) extends FunSuite with ShouldMatchers {

  type Sol = List[Int]

  case class SchedulingInstance(val durations: Array[Int], val demands: Array[Int], val resources: Array[Int], val capacity: Array[Int], val horizon: Int) {
    override def toString: String = {
      val dur = "durations: " + durations.mkString(", ")
      val dem = "demands: " + demands.mkString(", ")
      val res = "resources: " + resources.mkString(", ")
      val cap = "capacity: " + capacity.mkString(", ")
      val hor = "horizon: " + horizon
      dur + "\n" + dem + "\n" + res + "\n" + cap + "\n" + hor
    }
  }

  class CPSched(instance: SchedulingInstance) extends CPSolver {
    implicit val solver = this
    silent = true
    val nTasks = instance.demands.size
    val Tasks = 0 until nTasks
    val nResources = instance.capacity.size
    val Resources = 0 until nResources
    val durations = Array.tabulate(nTasks)(t => CPIntVar(instance.durations(t)))
    val starts = Array.tabulate(nTasks)(t => CPIntVar(0 to instance.horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPIntVar(durations(t).min to instance.horizon))
    val demands = Array.tabulate(nTasks)(t => CPIntVar(instance.demands(t)))
    val resources = Array.tabulate(nTasks)(t => CPIntVar(instance.resources(t)))
    Tasks.foreach(t => post(ends(t) == starts(t) + durations(t)))
  }

  def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint

  def generateRandomSchedulingProblem(nTasks: Int): SchedulingInstance = {
    val rand = new scala.util.Random()
    val durations = Array.fill(nTasks)(rand.nextInt(4))
    val demands = Array.fill(nTasks)(rand.nextInt(4))
    val capacity = Array(rand.nextInt(4) + 1)
    val resources = Array.fill(nTasks)(0)
    val horizon = rand.nextInt(durations.sum + 1 - durations.max) + durations.max
    SchedulingInstance(durations, demands, resources, capacity, horizon)
  }

  def solveAll(cp: CPSched, capacity: Array[Int], decomp: Boolean): Set[Sol] = {
    if (!decomp) cp.Resources.foreach(r => cp.post(cumulative(cp.starts, cp.durations, cp.ends, cp.demands, cp.resources, CPIntVar(capacity(r))(cp), r)))
    else cp.Resources.foreach(r => cp.post(new CumulativeDecomp(cp.starts, cp.durations, cp.ends, cp.demands, cp.resources, CPIntVar(capacity(r))(cp), r)))
    var sols: List[Sol] = List()

    cp.search {
      val b1 = new BinaryStaticOrderBranching(cp.starts)
      val b2 = new BinaryStaticOrderBranching(cp.durations)
      val b3 = new BinaryStaticOrderBranching(cp.resources)
      b1 ++ b2 ++ b3
    }

    cp.onSolution {
      val sol = cp.starts.map(_.value).toList
      sols = sol :: sols
    }

    cp.start()

    sols.toSet
  }

  def compare(sols1: Set[Sol], sols2: Set[Sol]): Boolean = {
    if (sols1.size != sols2.size) false
    else sols1.forall(s => sols2 contains s)
  }

  test("test solveAll " + cumulativeName) {
    (1 to nTests).forall(i => {
      //print("test " + cumulativeName + " instance " + i + ": ")
      val instance = generateRandomSchedulingProblem(5)
      val cpDecomp = new CPSched(instance)
      val cpCumul = new CPSched(instance)
      val allSolsDecomp = solveAll(cpDecomp, instance.capacity, true)
      val allSolsCumul = solveAll(cpCumul, instance.capacity, false)
      if (compare(allSolsDecomp, allSolsCumul)) {
        //println("success " + allSolsDecomp.size + " " + allSolsCumul.size)
        true
      } else {
        print("test " + cumulativeName + " instance " + i + ": ")
        println("failed !")
        println("expected number of solutions: " + allSolsDecomp.size)
        println("number of solutions: " + allSolsCumul.size)
        println("INSTANCE")
        println(instance)
        
        false
      }
    }) should be(true)
  }
}

class TestSweepMaxCumulative2 extends TestCumulativeConstraint("SweepMaxCumulative") {
  override def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint = {
    new SweepMaxCumulative(starts, durations, ends, demands, resources, capacity, id: Int)
  }
}

class TestEnergeticReasoning extends TestCumulativeConstraint("EnergeticReasoning") {
  override def cumulative(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], demands: Array[CPIntVar], resources: Array[CPIntVar], capacity: CPIntVar, id: Int): Constraint = {
    new EnergeticReasoning(starts, durations, ends, demands, resources, capacity, id: Int)
  }
}