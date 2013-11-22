package oscar.cbls.invariants.tests

import scala.collection.immutable.SortedSet
import org.scalacheck.Gen
import org.scalacheck.Gen.value
import org.scalacheck.Prop
import org.scalatest.PropSpec
import org.scalatest.prop.Checkers
import oscar.cbls.invariants.core.computation.IntSetVar
import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.invariants.core.computation.Model
import oscar.cbls.invariants.core.computation.Variable
import oscar.cbls.invariants.lib.logic.DenseCount
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.logic.IntElements
import oscar.cbls.invariants.lib.logic.IntITE
import oscar.cbls.invariants.lib.logic.IntSetElement
import oscar.cbls.invariants.lib.logic.SelectLEHeapHeap
import oscar.cbls.invariants.lib.logic.SelectLESetQueue
import oscar.cbls.invariants.lib.logic.Sort
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import oscar.cbls.invariants.lib.minmax.ArgMinArray
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.minmax.MaxSet
import oscar.cbls.invariants.lib.minmax.MinArray
import oscar.cbls.invariants.lib.minmax.MinSet
import oscar.cbls.invariants.lib.numeric.Abs
import oscar.cbls.invariants.lib.numeric.Div
import oscar.cbls.invariants.lib.numeric.Minus
import oscar.cbls.invariants.lib.numeric.Mod
import oscar.cbls.invariants.lib.numeric.Prod
import oscar.cbls.invariants.lib.numeric.Prod2
import oscar.cbls.invariants.lib.numeric.ProdElements
import oscar.cbls.invariants.lib.numeric.Step
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.invariants.lib.numeric.SumElements
import org.scalatest.FunSuite

class InvariantTests extends FunSuite with Checkers {

  test("AllDiff")(pending)

  test("AtLeast")(pending)

  test("AtMost")(pending)

  test("MultiKnapsack")(pending)

  test("Sequence")(pending)

  test("Access to ITE") {
    val bench = new InvariantTestBench
    new IntITE(bench.genIntVar(0 to 20), bench.genIntVar(0 to 20), bench.genIntVar(0 to 20)).toIntVar
    bench.run
  }

  test("Access to int var...") {
    val bench = new InvariantTestBench
    new IntElement(bench.genIntVar(0 to 19), bench.genIntVarsArray(20, 0 to 100)).toIntVar
    bench.run
  }

  test("Access to int vars...") {
    val bench = new InvariantTestBench(true)
    new IntElements(bench.genIntSetVar(0 to 4, 3), bench.genIntVarsArray(5, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Access to int set var...") {
    val bench = new InvariantTestBench
    new IntSetElement(bench.genIntVar(0 to 19), bench.genIntSetVars(20, 10, 0 to 100)).toIntSetVar
    bench.run
  }

  test("Clusters...")(pending)

  ignore("Dense Count...") {
    val bench = new InvariantTestBench
    new DenseCount(bench.genIntVarsArray(10, 0 to 19), bench.genIntVarsArray(20, 0 to 10))
    bench.run
  }

  test("Cross references...")(pending)

  test("Cumulative...")(pending)

  test("Filter...")(pending)

  test("SelectLEHeapHeap") {
    val bench = new InvariantTestBench
    new SelectLEHeapHeap(bench.genIntVarsArray(20, 0 to 30), bench.genIntVar(30 to 100)).toIntSetVar
    bench.run
  }

  test("SelectLESetQueue") {
    val bench = new InvariantTestBench
    new SelectLESetQueue(bench.genIntVarsArray(20, 0 to 30), bench.genIntVar(30 to 100)).toIntSetVar
    bench.run
  }

  test("Predecessor")(pending)

  test("Routes")(pending)

  test("Sort") {
    val bench = new InvariantTestBench
    new Sort(bench.genIntVarsArray(20, 0 to 30), bench.genIntVarsArray(20, 30 to 100))
    bench.run
  }

  test("ArgMaxArray") {
    val bench = new InvariantTestBench
    new ArgMaxArray(bench.genIntVarsArray(20, 0 to 30), bench.genIntSetVar(0 to 30, 3)).toIntSetVar
    bench.run
  }

  test("ArgMinArray") {
    val bench = new InvariantTestBench
    new ArgMinArray(bench.genIntVarsArray(20, 0 to 30), bench.genIntSetVar(0 to 30, 3)).toIntSetVar
    bench.run
  }

  test("MaxLin")(pending)

  test("MinLin")(pending)

  test("Min")(pending)

  test("Max")(pending)

  test("Min2")(pending)

  test("Max2")(pending)

  test("MinArray maintains min") {
    val bench = new InvariantTestBench
    new MinArray(bench.genIntVarsArray(4, 0 to 100)).toIntVar
    //    val minVar = new MinArray(bench.genIntVarsArray(4, 0 to 100))
    //    Event(minVar, { println("Trigger : changed " + minVar) })
    bench.run
  }

  test("MaxArray maintains max") {
    val bench = new InvariantTestBench
    new MaxArray(bench.genIntVarsArray(2, 0 to 50)).toIntVar
    //    val maxVar = new MaxArray(bench.genIntVarsArray(2, 0 to 50))
    //    Event(maxVar, { println("Trigger : changed " + maxVar) })
    bench.run
  }

  test("MinSet") {
    val bench = new InvariantTestBench
    new MinSet(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }

  test("MaxSet") {
    val bench = new InvariantTestBench
    new MaxSet(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }

  test("SumElements") {
    val bench = new InvariantTestBench
    new SumElements(bench.genIntVarsArray(10, 0 to 100), bench.genIntSetVar(0 to 9, 5)).toIntVar
    bench.run
  }

  test("ProdElements") {
    val bench = new InvariantTestBench
    new ProdElements(bench.genIntVarsArray(10, 0 to 100), bench.genIntSetVar(0 to 9, 5)).toIntVar
    bench.run
  }

  test("Sum") {
    val bench = new InvariantTestBench
    new Sum(bench.genIntVarsArray(10, 0 to 100)).toIntVar
    bench.run
  }

  test("Prod") {
    val bench = new InvariantTestBench
    new Prod(bench.genIntVarsArray(10, 0 to 100)).toIntVar
    bench.run
  }

  test("Minus") {
    val bench = new InvariantTestBench
    new Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Sum2") {
    val bench = new InvariantTestBench
    new Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Prod2") {
    val bench = new InvariantTestBench
    new Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Div") {
    val bench = new InvariantTestBench
    new Div(bench.genIntVar(0 to 100), bench.genIntVar(1 to 100)).toIntVar
    bench.run
  }

  test("Mod") {
    val bench = new InvariantTestBench
    new Mod(bench.genIntVar(0 to 100), bench.genIntVar(1 to 100)).toIntVar
    bench.run
  }

  test("Abs") {
    val bench = new InvariantTestBench
    new Abs(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  test("Step") {
    val bench = new InvariantTestBench
    new Step(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  test("RoundUpModulo")(pending)

  test("RoundUpCustom")(pending)

  test("Union")(pending)

  test("Inter")(pending)

  test("Diff")(pending)

  test("Cardinality")(pending)

  test("MakeSet")(pending)

  test("Interval")(pending)

  test("TakeAny")(pending)

  test("SetSum")(pending)

  test("SetProd")(pending)
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
      case Random() => {
        randomVar := Gen.choose(randomVar.minVal, randomVar.maxVal).sample.get
      }
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

class InvariantTestBench(verbose: Boolean = false) {
  var property: Prop = false
  val checker = new InvariantChecker(verbose)
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

    try {
      property = org.scalacheck.Prop.forAll(InvGen.move) {
        randomMove: Move =>
          if (verbose) {
            if (randomVars.length > 1) randomVars.foreach((rv: RandomVar) => println(rv.toString()))
            print(randomMove.toString() + " ")
          }
          val randomVar = Gen.oneOf(randomVars).sample.get
          if (verbose) print(randomVar.toString() + " => ")
          randomVar.move(randomMove)
          if (verbose) println(randomVar.toString())
          model.propagate()
          if (verbose) println
          checker.isChecked
      }
    } catch {
      case e: Exception =>
        println("Exception caught: " + e)
    }
    Checkers.check(property)
  }
}
