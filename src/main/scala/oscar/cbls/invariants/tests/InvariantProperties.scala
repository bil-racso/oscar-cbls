package oscar.cbls.invariants.tests

import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.propBoolean
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import oscar.cbls.invariants.core.computation.IntInvariant.toIntVar
import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.invariants.core.computation.Invariant
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.minmax.MiaxArray
import oscar.cbls.invariants.lib.minmax.MinArray
import oscar.cbls.invariants.core.computation.Variable
import oscar.cbls.invariants.core.computation.IntSetVar
import oscar.cbls.invariants.core.computation.Event

class InvariantProperties extends PropSpec with PropertyChecks {

  property("MaxArray maintains max") {
    val bench = new InvariantCheck(Gen.choose(0, 100))
    val maxVar = new MaxArray(bench.genVarsArray(4, 0 to 100))
    Event(maxVar, { println("Trigger : changed : " + maxVar) })
    bench.run
  }
}

class Generators {
}

abstract class RandomVar {
  def getRandomVar(): Variable

  def move(value: Int)
}

class RandomIntVar(intVar: IntVar) extends RandomVar {
  val randomVar = intVar

  override def getRandomVar(): IntVar = randomVar

  override def move(value: Int) {
    randomVar := value
    println("Changed " + randomVar.name + " to " + value)
  }
}

class RandomIntSetVar(intSetVar: IntSetVar) extends RandomVar {
  val randomVar = intSetVar

  override def getRandomVar(): IntSetVar = randomVar

  override def move(value: Int) {
  }
}

class InvariantCheck(gen: Gen[Int]) {
  val checker = new InvariantChecker
  val model = new Model(false, Some(checker))
  var vars: Array[RandomVar] = new Array[RandomVar](0)

  def genVarsArray(nbVars: Int, intVarRange: Range): Array[IntVar] = {
    val minMaxValueGen = Gen.choose(intVarRange.min, intVarRange.max)

    val intVarGenerator = for {
      n <- minMaxValueGen
      c <- Gen.alphaChar
    } yield new RandomIntVar(new IntVar(model, intVarRange, n, c.toString))

    val genIntVarList = Gen.containerOfN[List, RandomIntVar](nbVars, intVarGenerator)

    vars = genIntVarList.sample.get.toArray
    vars.map((randomVar: RandomVar) =>
      randomVar match {
        case rv: RandomIntVar => {
          println(rv.getRandomVar.toString)
          rv.getRandomVar
        }
        case _ => throw new IllegalArgumentException("Wrong type.")
      })
  }

  def run() = {
    model.close()
    println("Model closed")
    model.propagate()

    val property: Prop = org.scalacheck.Prop.forAll(gen) {
      value: Int =>
        Gen.oneOf(vars).sample.get.move(value)
        model.propagate()
        checker.isChecked
    }

    property.check
  }
}
