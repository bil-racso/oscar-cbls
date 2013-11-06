package oscar.cbls.invariants.tests

import org.scalacheck.Gen
import org.scalacheck.Prop.propBoolean
import org.scalatest.PropSpec
import oscar.cbls.invariants.core.computation.Event
import oscar.cbls.invariants.core.computation.IntInvariant.toIntVar
import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.invariants.core.computation.Invariant
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.invariants.lib.minmax.MaxArray
import org.scalatest.prop.PropertyChecks
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.core.propagation.Checker

class InvariantProperties extends PropSpec with PropertyChecks {
  
  var invariantChecked = true

  class InvariantChecker extends Checker {
    override def check(verity: Boolean, traceOption: Option[String] = None) = {
      if (traceOption.isDefined && !verity)
        println("Counter-example found: " + traceOption.getOrElse(""))
      invariantChecked = invariantChecked && verity
    }
  }

  def changeValue(model: Model, inv: Invariant, intVar: IntVar, value: Int): Boolean = {
    intVar := value
    println("Changed " + intVar.name + " to " + value)
    model.propagate()
    invariantChecked
  }

  val min = 0
  val max = 100
  val model = new Model(false, Some(new InvariantChecker))

  val minMaxValueGen = Gen.choose(min, max)

  val intVarGenerator = for {
    n <- minMaxValueGen
    c <- Gen.alphaChar
  } yield IntVar(model, min, max, n, c.toString)

  val genIntVarList = Gen.containerOfN[List, IntVar](4, intVarGenerator)

  val vars = genIntVarList.sample.get.toArray

  vars.foreach(v => println(v.getValue()))

  val maxInv: MaxArray = MaxArray(vars)
  val maxVar: IntVar = maxInv

  Event(maxVar, { println("Trigger : changed : " + maxVar) })

  model.close()
  println("Model closed")
  model.propagate()

  property("MaxArray maintains max") {
    val p = org.scalacheck.Prop.forAll(minMaxValueGen) {
      value: Int =>
        changeValue(model, maxInv, Gen.oneOf(vars).sample.get, value)
    }
    p.check
  }
}
