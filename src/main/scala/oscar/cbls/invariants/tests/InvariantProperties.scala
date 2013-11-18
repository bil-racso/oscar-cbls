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
import oscar.cbls.invariants.lib.logic.IntITE
import scala.collection.immutable.SortedSet
import oscar.cbls.invariants.lib.logic.IntElements
import oscar.cbls.invariants.lib.logic.IntSetElement
import oscar.cbls.invariants.lib.logic.DenseCount
import oscar.cbls.invariants.lib.logic.SelectLEHeapHeap
import oscar.cbls.invariants.lib.logic.SelectLESetQueue
import oscar.cbls.invariants.lib.logic.Sort
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import oscar.cbls.invariants.lib.minmax.ArgMinArray
import oscar.cbls.invariants.lib.minmax.MinSet
import oscar.cbls.invariants.lib.minmax.MaxSet
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.invariants.lib.numeric.ProdElements
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.numeric.Prod
import oscar.cbls.invariants.lib.numeric.Minus
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.invariants.lib.numeric.Prod2
import oscar.cbls.invariants.lib.numeric.Div
import oscar.cbls.invariants.lib.numeric.Mod
import oscar.cbls.invariants.lib.numeric.Abs
import oscar.cbls.invariants.lib.numeric.Step

class InvariantProperties extends PropSpec with PropertyChecks {

  property("Access to ITE") {
    val bench = new InvariantCheck
    new IntITE(bench.genIntVar(0 to 20), bench.genIntVar(0 to 20), bench.genIntVar(0 to 20)).toIntVar
    bench.run
  }

  property("Access to int var...") {
    val bench = new InvariantCheck
    new IntElement(bench.genIntVar(0 to 19), bench.genIntVarsArray(20, 0 to 100)).toIntVar
    bench.run
  }

  property("Access to int vars...") {
    val bench = new InvariantCheck
    new IntElements(bench.genIntSetVar(0 to 19, 5), bench.genIntVarsArray(20, 0 to 100)).toIntSetVar
    bench.run
  }

  property("Access to int set var...") {
    val bench = new InvariantCheck
    new IntSetElement(bench.genIntVar(0 to 19), bench.genIntSetVars(20, 10, 0 to 100)).toIntSetVar
    bench.run
  }

  property("Clusters...")(pending)

  ignore("Dense Count...") {
    val bench = new InvariantCheck
    new DenseCount(bench.genIntVarsArray(10, 0 to 19), bench.genIntVarsArray(20, 0 to 10))
    bench.run
  }

  property("Cross references...")(pending)

  property("Cumulative...")(pending)

  property("Filter...")(pending)
  
  property("SelectLEHeapHeap") {
    val bench = new InvariantCheck
    new SelectLEHeapHeap(bench.genIntVarsArray(20, 0 to 30), bench.genIntVar(30 to 100)).toIntSetVar
    bench.run
  }
  
  property("SelectLESetQueue") {
    val bench = new InvariantCheck
    new SelectLESetQueue(bench.genIntVarsArray(20, 0 to 30), bench.genIntVar(30 to 100)).toIntSetVar
    bench.run
  }
  
  property("Predecessor")(pending)
  
  property("Routes")(pending)
  
  property("Sort") {
    val bench = new InvariantCheck
    new Sort(bench.genIntVarsArray(20, 0 to 30), bench.genIntVarsArray(20, 30 to 100))
    bench.run
  }
  
  property("ArgMaxArray") {
    val bench = new InvariantCheck
    new ArgMaxArray(bench.genIntVarsArray(20, 0 to 30), bench.genIntSetVar(0 to 30, 3)).toIntSetVar
    bench.run
  }
  
  property("ArgMinArray") {
    val bench = new InvariantCheck
    new ArgMinArray(bench.genIntVarsArray(20, 0 to 30), bench.genIntSetVar(0 to 30, 3)).toIntSetVar
    bench.run
  }
  
  property("MaxLin")(pending)
  
  property("MinLin")(pending)
  
  property("Min")(pending)
  
  property("Max")(pending)
  
  property("Min2")(pending)
  
  property("Max2")(pending)

  property("MinArray maintains min") {
    val bench = new InvariantCheck
    new MinArray(bench.genIntVarsArray(4, 0 to 100)).toIntVar
    //    val minVar = new MinArray(bench.genIntVarsArray(4, 0 to 100))
    //    Event(minVar, { println("Trigger : changed " + minVar) })
    bench.run
  }

  property("MaxArray maintains max") {
    val bench = new InvariantCheck
    new MaxArray(bench.genIntVarsArray(2, 0 to 50)).toIntVar
    //    val maxVar = new MaxArray(bench.genIntVarsArray(2, 0 to 50))
    //    Event(maxVar, { println("Trigger : changed " + maxVar) })
    bench.run
  }
  
  property("MinSet") {
    val bench = new InvariantCheck
    new MinSet(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }
  
  property("MaxSet") {
    val bench = new InvariantCheck
    new MaxSet(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }
  
  property("SumElements") {
    val bench = new InvariantCheck
    new SumElements(bench.genIntVarsArray(10, 0 to 100), bench.genIntSetVar(0 to 9, 5)).toIntVar
    bench.run
  }
  
  property("ProdElements") {
    val bench = new InvariantCheck
    new ProdElements(bench.genIntVarsArray(10, 0 to 100), bench.genIntSetVar(0 to 9, 5)).toIntVar
    bench.run
  }
  
  property("Sum") {
    val bench = new InvariantCheck
    new Sum(bench.genIntVarsArray(10, 0 to 100)).toIntVar
    bench.run
  }
  
  property("Prod") {
    val bench = new InvariantCheck
    new Prod(bench.genIntVarsArray(10, 0 to 100)).toIntVar
    bench.run
  }
  
  property("Minus") {
    val bench = new InvariantCheck
    new Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }
  
  property("Sum2") {
    val bench = new InvariantCheck
    new Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }
  
  property("Prod2") {
    val bench = new InvariantCheck
    new Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }
  
  property("Div") {
    val bench = new InvariantCheck
    new Div(bench.genIntVar(0 to 100), bench.genIntVar(1 to 100)).toIntVar
    bench.run
  }
  
  property("Mod") {
    val bench = new InvariantCheck
    new Mod(bench.genIntVar(0 to 100), bench.genIntVar(1 to 100)).toIntVar
    bench.run
  }
  
  property("Abs") {
    val bench = new InvariantCheck
    new Abs(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }
  
  property("Step") {
    val bench = new InvariantCheck
    new Step(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }
  
  property("RoundUpModulo")(pending)
  
  property("RoundUpCustom")(pending)
  
  property("Union")(pending)
  
  property("Inter")(pending)
  
  property("Diff")(pending)
  
  property("Cardinality")(pending)
  
  property("MakeSet")(pending)
  
  property("Interval")(pending)
  
  property("TakeAny")(pending)
  
  property("SetSum")(pending)
  
  property("SetProd")(pending)
}

abstract class Move

case class PlusOne extends Move
case class MinusOne extends Move
case class ToZero extends Move
case class ToMin extends Move
case class ToMax extends Move
case class Random extends Move

object InvGen {
  val move = Gen.oneOf(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random())

  def randomValue(range: Range) = Gen.choose(range.min, range.max)

  def randomIntVar(range: Range, model: Model) = for {
    v <- randomValue(range)
    c <- Gen.alphaChar
  } yield new RandomIntVar(new IntVar(model, range, v, c.toString))

  def randomIntVars(nbVars: Int, range: Range, model: Model) = {
    Gen.containerOfN[List, RandomIntVar](nbVars, randomIntVar(range, model))
  }

  def randomFixedIntSetVar(range: Range, size: Int, model: Model) = for {
    c <- Gen.alphaChar
    v <- Gen.containerOfN[List, Int](size, randomValue(range))
  } yield new RandomIntSetVar(new IntSetVar(model, range.min, range.max, c.toString, SortedSet(v: _*)))

  def randomIntSetVar(range: Range, upToSize: Int, model: Model) = for {
    c <- Gen.alphaChar
    s <- randomValue(1 to upToSize)
    v <- Gen.containerOfN[List, Int](s, randomValue(range))
  } yield new RandomIntSetVar(new IntSetVar(model, range.min, range.max, c.toString, SortedSet(v: _*)))

  def randomIntSetVars(nbVars: Int, upToSize: Int, range: Range, model: Model) = {
    Gen.containerOfN[List, RandomIntSetVar](nbVars, randomIntSetVar(range, upToSize, model))
  }
}

abstract class RandomVar {
  def randomVar(): Variable

  def move(move: Move)
}

case class RandomIntVar(intVar: IntVar) extends RandomVar {
  override def randomVar(): IntVar = intVar

  override def move(move: Move) = {
    move match {
      case PlusOne() => {
        //print(randomVar.name + " :+= " + 1)
        if (randomVar.domain.contains(randomVar.value + 1)) randomVar :+= 1
        else randomVar := randomVar.minVal
      }
      case MinusOne() => {
        //print(randomVar.name + " :-= " + 1)
        if (randomVar.domain.contains(randomVar.value - 1)) randomVar :-= 1
        else randomVar := randomVar.maxVal
      }
      case ToZero() => {
        //print(randomVar.name + " := " + 0)
        randomVar := 0
      }
      case ToMax() => {
        //print(randomVar.name + " := " + randomVar.maxVal)
        randomVar := randomVar.maxVal
      }
      case ToMin() => {
        //print(randomVar.name + " := " + randomVar.minVal)
        randomVar := randomVar.minVal
      }
      case Random() => Gen.choose(randomVar.minVal, randomVar.maxVal)
    }
    //println(" (" + randomVar.name + " := " + randomVar.value + ")")
  }
}

case class RandomIntSetVar(intSetVar: IntSetVar) extends RandomVar {
  override def randomVar(): IntSetVar = intSetVar

  override def move(move: Move) = {
    move match {
      case PlusOne() => {
        randomVar :+= InvGen.randomValue(randomVar.getMinVal to randomVar.getMaxVal).sample.get
      }
      case MinusOne() => {
        if (!randomVar.value.isEmpty) randomVar :-= Gen.oneOf(randomVar.value.toSeq).sample.get
        //else randomVar.value = Seq.fill(randomVar.value.size)(util.Random.nextInt)
      }
      case ToZero() => {
        randomVar.value.foreach(value => randomVar.deleteValue(value))
      }
      case ToMax() => // TODO
      case ToMin() => // TODO
      case Random() => // TODO
    }
  }
}

class InvariantCheck {
  val checker = new InvariantChecker
  val model = new Model(false, Some(checker))
  var randomVars: List[RandomVar] = List()

  def genIntVar(range: Range): IntVar = {
    val riVar = InvGen.randomIntVar(range, model).sample.get
    randomVars = riVar :: randomVars
    riVar.randomVar
  }

  def genIntVarsArray(nbVars: Int = 4, range: Range = 0 to 100): Array[IntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model).sample.get
    randomVars = riVars ::: randomVars
    riVars.map((riv: RandomIntVar) => {
      //println(riv.getRandomVar.toString)
      riv.randomVar
    }).toArray
  }

  def genIntSetVar(range: Range, size: Int) = {
    val risVar = InvGen.randomFixedIntSetVar(range, size, model).sample.get
    randomVars = risVar :: randomVars
    risVar.randomVar
  }

  def genIntSetVars(nbVars: Int = 4, upToSize: Int = 20, range: Range = 0 to 100): Array[IntSetVar] = {
    val risVars = InvGen.randomIntSetVars(nbVars, upToSize, range, model).sample.get
    randomVars = risVars ::: randomVars
    risVars.map((risv: RandomIntSetVar) => {
      risv.randomVar
    }).toArray
  }

  def run() = {
    model.close()
    //println("Model closed")
    model.propagate()

    var property: Prop = false

    try {
      property = org.scalacheck.Prop.forAll(InvGen.move) {
        randomMove: Move =>
          val randomVar = Gen.oneOf(randomVars).sample.get
          randomVar.move(randomMove)
          model.propagate()
          checker.isChecked
      }
    } catch {
      case e: Exception =>
        println("Exception caught: " + e)
        false
    }
    property.check
  }
}
