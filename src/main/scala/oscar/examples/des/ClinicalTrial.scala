package oscar.examples.des

import oscar.stochastic._
import oscar.invariants._
import oscar.des.engine._
import JSci.maths.statistics._
import scala.util.continuations._
import JSci.maths.statistics.ExponentialDistribution
import scala.collection._

abstract case class Phase() {
  val pDropOut: Double
}
case class Phase1 extends Phase {
  val pDropOut = 0.1
}

case class Phase2 extends Phase {
  val pDropOut = 0.3
}

trait Indexable {
  val index: Int
}

case class Dose(qty: Int)
case class DropOut extends Dose(-1)

abstract class Visit(index: Int) {

}

abstract class IntermediateVisit(index: Int) extends Visit(index) {
  def nextVisitTime: Distr[Double]
  def next: Distr[Visit]
}

case class DispensingVisit(index: Int, dl: Dose) extends IntermediateVisit(index) {

  val phase1Interval: Distr[Double] = new ExponentialDistribution(1.0 / 15)
  val phase2Interval: Distr[Double] = new ExponentialDistribution(1.0 / 45)

  val titration = new Choice((List((0.6, dl), (0.4, dl match { case Dose(i) => Dose(3 - i) }))))
  def phase = if (index < 5) Phase1() else Phase2()

  def qtyDrug = {
    dl match {
      case Dose(1) => if (phase == Phase1()) 2 else 5
      case Dose(2) => if (phase == Phase1()) 4 else 10
    }
  }
  def nextVisitTime = phase match {
    case Phase1() => phase1Interval
    case Phase2() => phase2Interval
    case _        => new ValueDistr(5.0)
  }
  def next = if (index == 10) FollowUp(10) else titration.map(DispensingVisit(index + 1, _))

}
case class FollowUp(index: Int) extends Visit(index) {

}

abstract case class TreatmentArm {
  def pt(qty: Int): List[(Int, PackageType)]
}
case class TA1() extends TreatmentArm {
  def pt(qty: Int) = List((qty, Placebo()))
}
case class TA2() extends TreatmentArm {
  def pt(qty: Int) = List((qty, DrugA()))
}

abstract case class PackageType extends Indexable {}
case class Placebo() extends PackageType {
  val index = 1
}
case class DrugA() extends PackageType {
  val index = 0
}

class Patient(m: Model, site: Site, name: String) extends Process(m, "Patient") {

  val randomizationInterval = new NumberGenerator(new NormalDistribution(30, 4))
  val phase1Interval = new NumberGenerator(new NormalDistribution(15, 2))
  val phase2Interval = new NumberGenerator(new NormalDistribution(45, 2))
  val screeningFailure = new Flip(0.15)
  val treatmentArms = new Choice(List((0.4, TA1()), (0.6, TA2())))

  var ta: TreatmentArm = null

  def screening() = {
    m.print(name + " screening")
    if (screeningFailure()) {
      // screening DispensingVisit failed
      m.print(name + " failed screening")
    } else {
      m.wait(randomizationInterval())
      randomization()
    }
  }

  def randomization() = {
    m.print(name + " is randomized at time ");
    ta = treatmentArms()
    m.wait(phase1Interval())
    visit(DispensingVisit(1, Dose(1)))
  }

  def visit(v: DispensingVisit): Unit @suspendable = {
    m.print(name + " at visit " + v.index)
    if (!site.consume(ta.pt(v.qtyDrug))) {
      // OOD
    } else {

    }
    m.wait(v.nextVisitTime())
    val n = v.next()
    n match {
      case a: FollowUp        => followUp(a)
      case a: DispensingVisit => visit(a)
    }
  }

  def followUp(v: FollowUp) {

  }

  def start = screening
}

class Site(m: Model, name: String) extends Process(m, "Site") {

  type Demand = TableFunction[Int]

  val demand = new Demand()(IntOp)
  val inventory: Array[Int] = Array(500, 250)

  def start() = {
    m.print("Site opening")
    val self = this
    var pIndex = 0
    val patientGenerator = Generator(m, new ExponentialDistribution(1.0 / 30)) {
      val p = new Patient(m, self, "Patient-" + pIndex + ":" + name)
      pIndex += 1
      p.simulate()
    }
    waitFor(m.clock === 1001)

    //println(name + " demand  = " + demand);
    ()
  }

  def consume(order: List[(Int, PackageType)]): Boolean = {
    val res = order.foldLeft(true) { case (b, (qty, pt)) => b && qty <= inventory(pt.index) }

    if (res) {
      for ((qty, pt) <- order) {
        inventory(pt.index) -= qty
        demand(m.clock().floor.toInt) = qty
      }
    }
    res

  }
}

class ClinicalTrial(m: Model) extends Process(m, "CT") {

  var Lsites = new immutable.ListSet[Site]()

  override def start() = {
    var id = 0
    val sites = Generator(m, new ExponentialDistribution(1.0 / 90)) {

      val s = new Site(m, "Site-" + id)
      Lsites += s
      s.simulate()
      id += 1
    }
  }
}

import akka.dispatch.Await
import akka.dispatch.Future
import oscar.distributed._

import akka.util.Duration._
import akka.util.duration._

object ClinicalTrial {
  def main(args: Array[String]) {
    val distribute = new DistributedComputation[TableFunction[Int]](4)

    val results = for (i <- 1 to 10) yield distribute {
      val m = new Model()

      val ct = new ClinicalTrial(m)

      m.simulate(1010, false)

      TableFunction.sum(for (s <- ct.Lsites) yield s.demand)(IntOp)
    }

    val demand = Await.result(distribute.fold(results)(new LearnedNumericalFunction(0.0,0.0)){ (f,d)=>
      f.observe(d)
      f
    },10 second)

    val mean = {t:Int=> demand.mean(t)}
    implicit def intOp:Operationable[Int] = IntOp
    println("Total demand: " + Await.result(distribute.reduce(results)(_+_), 10 second))
    val m = for( (t,v) <- demand ) yield{"("+t+","+demand.mean(t)+")"}
    println("Mean  demand: {" + m + ")")
    

  }

}

