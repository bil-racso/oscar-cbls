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
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.logic.IntElement

class InvariantProperties extends PropSpec with PropertyChecks {

  property("MinArray maintains min") {
    val bench = new InvariantCheck
    new MinArray(bench.genIntVarsArray(4, 0 to 100)).toIntVar
    //    val minVar = new MinArray(bench.getIntVars)
    //    Event(minVar, { println("Trigger : changed " + minVar) })
    bench.run
  }

  property("MaxArray maintains max") {
    val bench = new InvariantCheck
    new MaxArray(bench.genIntVarsArray(2, 0 to 50)).toIntVar
    //    val maxVar = new MaxArray(bench.getIntVars)
    //    Event(maxVar, { println("Trigger : changed " + maxVar) })
    bench.run
  }

  property("Access to int element...") {
    val bench = new InvariantCheck
    new IntElement(bench.genIntVar(0 to 20), bench.genIntVarsArray(20, 0 to 100)).toIntVar
    bench.run
  }
}

abstract class Move

case class PlusOne extends Move
case class MinusOne extends Move
case class ToZero extends Move
case class ToMin extends Move
case class ToMax extends Move
case class Value(value: Int) extends Move

object InvGen {
  val move = Gen.oneOf(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax())

  def randomValue(range: Range) = Gen.choose(range.min, range.max)

  def randomIntVar(range: Range, model: Model) = for {
    n <- randomValue(range)
    c <- Gen.alphaChar
  } yield new RandomIntVar(new IntVar(model, range, n, c.toString))

  def randomIntVars(nbVars: Int, range: Range, model: Model) = {
    Gen.containerOfN[List, RandomIntVar](nbVars, randomIntVar(range, model))
  }
}

abstract class RandomVar {
  def getRandomVar(): Variable

  def move(move: Move)
}

case class RandomIntVar(intVar: IntVar) extends RandomVar {
  val randomVar = intVar

  override def getRandomVar(): IntVar = randomVar

  override def move(move: Move) = {
    move match {
      case PlusOne() => {
        print(randomVar.name + " :+= " + 1)
        randomVar :+= 1
      }
      case MinusOne() => {
        print(randomVar.name + " :-= " + 1)
        randomVar := randomVar.value - 1
      }
      case ToZero() => {
        print(randomVar.name + " := " + 0)
        randomVar := 0
      }
      case ToMax() => {
        print(randomVar.name + " := " + randomVar.maxVal)
        randomVar := randomVar.maxVal
      }
      case ToMin() => {
        print(randomVar.name + " := " + randomVar.minVal)
        randomVar := randomVar.minVal
      }
      case Value(value: Int) => randomVar := value
    }
    println(" (" + randomVar.name + " := " + randomVar.value + ")")
  }
}

abstract case class RandomIntSetVar(intSetVar: IntSetVar) extends RandomVar {
  val randomVar = intSetVar

  override def getRandomVar(): IntSetVar = randomVar

  //  override def move(move: Move) = move match {
  //    case PlusOne() => randomVar := randomVar.insertValue(Gen.choose(randomVar.getMinVal, randomVar.getMaxVal))
  //    case MinusOne() => randomVar :-= Gen.oneOf(randomVar.value)
  //    case ToZero() => randomVar := 
  //    case ToMax() => randomVar := 
  //    case ToMin() => randomVar := 
  //    case Value(value: Int) => 
  //  }
}

class InvariantCheck {
  val checker = new InvariantChecker
  val model = new Model(false, Some(checker))
  var randomIntVars: List[RandomIntVar] = List()
  var randomIntSetVars: List[RandomIntSetVar] = List()

  def genIntVar(range: Range): IntVar = {
    val riVar = InvGen.randomIntVar(range, model).sample.get
    randomIntVars = riVar :: randomIntVars
    riVar.getRandomVar
  }

  def genIntVarsArray(nbVars: Int = 4, range: Range = 0 to 100): Array[IntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model).sample.get
    randomIntVars = riVars ::: randomIntVars
    randomIntVars.map((riv: RandomIntVar) => {
      //println(riv.getRandomVar.toString)
      riv.getRandomVar
    }).toArray
  }

  def run() = {
    model.close()
    //println("Model closed")
    model.propagate()

    val property: Prop = org.scalacheck.Prop.forAll(InvGen.move) {
      randomMove: Move =>
        val randomVar = Gen.oneOf(randomIntVars).sample.get
        randomVar.move(randomMove)
        model.propagate()
        checker.isChecked
    }

    property.check
  }
}
