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
import oscar.cbls.invariants.lib.minmax.Max
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.minmax.MaxSet
import oscar.cbls.invariants.lib.minmax.Min
import oscar.cbls.invariants.lib.minmax.Min2
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
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.invariants.lib.minmax.MaxLin
import oscar.cbls.invariants.lib.minmax.MinLin
import oscar.cbls.invariants.lib.set.Union
import oscar.cbls.invariants.lib.set.Inter
import oscar.cbls.invariants.lib.set.Diff
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.invariants.lib.set.MakeSet
import oscar.cbls.invariants.lib.set.Interval
import oscar.cbls.invariants.lib.set.TakeAny
import oscar.cbls.invariants.lib.set.SetSum
import oscar.cbls.invariants.lib.set.SetProd

class InvariantTests extends FunSuite with Checkers {

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvariantTestBench
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }

  test("AtLeast")(pending)

  test("AtMost")(pending)

  test("MultiKnapsack")(pending)

  test("Sequence")(pending)

  test("Access to ITE maintains output = if ifVar > 0 then thenVar else elseVar") {
    val bench = new InvariantTestBench
    new IntITE(bench.genIntVar(-2 to 3), bench.genIntVar(1 to 2), bench.genIntVar(10 to 11)).toIntVar
    bench.run
  }

  test("Access to int element maintains output = array(index)") {
    val bench = new InvariantTestBench
    new IntElement(bench.genIntVar(0 to 19), bench.genIntVarsArray(20, 0 to 100)).toIntVar
    bench.run
  }

  test("Access to int vars...") {
    val bench = new InvariantTestBench(2)
    new IntElements(bench.genIntSetVar(0 to 4, 3), bench.genIntVarsArray(5, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Access to int set element maintains output = array(index)") {
    val bench = new InvariantTestBench
    new IntSetElement(bench.genIntVar(0 to 19), bench.genIntSetVars(20, 10, 0 to 100)).toIntSetVar
    bench.run
  }

  test("Sparse Cluster...")(pending)

  test("Dense Cluster...")(pending)

  test("Dense Count maintains count(j) = #{i in index of values | values[i] == j}") {
    val bench = new InvariantTestBench
    new DenseCount(bench.genIntVarsArray(10, 0 to 19), bench.genIntVarsArray(20, 0 to 19, false))
    bench.run
  }

  test("Cross references...")(pending)

  test("Cumulative...")(pending)

  test("Filter...")(pending)

  test("SelectLEHeapHeap") {
    val bench = new InvariantTestBench(2)
    new SelectLEHeapHeap(bench.genIntVarsArray(4, 0 to 5), bench.genIntVar(3 to 10)).toIntSetVar
    bench.run
  }

  ignore("SelectLESetQueue") { //TODO exclure les changements de valeurs interdits
    val bench = new InvariantTestBench(2)
    new SelectLESetQueue(bench.genIntVarsArray(5, 0 to 5), bench.genIntVar(3 to 10, false)).toIntSetVar
    bench.run
  }

  test("Predecessor")(pending)

  test("Routes")(pending)

  test("Sort") {
    val bench = new InvariantTestBench(2)
    new Sort(bench.genIntVarsArray(6, 0 to 30), bench.genIntVarsArray(6, 0 to 5))
    bench.run
  }

  // TODO test also with the other parameters of ArgMinArray
  test("ArgMinArray maintains the set of min variables of the array") {
    val bench = new InvariantTestBench
    new ArgMinArray(bench.genIntVarsArray(20, 0 to 30)).toIntSetVar
    bench.run
  }

  // TODO test also with the other parameters of ArgMaxArray
  test("ArgMaxArray maintains the set of max variables of the array") {
    val bench = new InvariantTestBench(2)
    new ArgMaxArray(bench.genIntVarsArray(20, 0 to 30)).toIntSetVar
    bench.run
  }

  test("MaxLin") {
    val bench = new InvariantTestBench(2)
    new MaxLin(bench.genSortedIntVars(6, -10 to 10)).toIntVar
    bench.run
  }

  test("MinLin") {
    val bench = new InvariantTestBench(2)
    new MinLin(bench.genSortedIntVars(6, 0 to 10)).toIntVar
    bench.run
  }

  test("Min") {
    val bench = new InvariantTestBench(2)
    new Min(bench.genSortedIntVars(5, -10 to 10)).toIntVar
    bench.run
  }

  test("Max") {
    val bench = new InvariantTestBench(2)
    new Max(bench.genSortedIntVars(5, -10 to 10)).toIntVar
    bench.run
  }

  test("Min2") {
    val bench = new InvariantTestBench(2)
    new Min2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("Max2") {
    val bench = new InvariantTestBench(2)
    new Max2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("MinArray maintains min") {
    val bench = new InvariantTestBench
    new MinArray(bench.genIntVarsArray(4, 0 to 100)).toIntVar
    bench.run
  }

  test("MaxArray maintains max") {
    val bench = new InvariantTestBench
    new MaxArray(bench.genIntVarsArray(2, 0 to 50)).toIntVar
    bench.run
  }

  test("MinSet maintains the minimum of a set.") {
    val bench = new InvariantTestBench
    new MinSet(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }

  test("MaxSet maintains the maximum of a set") {
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

  // FIXME problem when too much variables
  test("Prod maintains the product of input vars.") {
    val bench = new InvariantTestBench(2)
    new Prod(bench.genIntVarsArray(3, 0 to 100)).toIntVar
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

  test("Div maintains the division of two integers") {
    val bench = new InvariantTestBench
    new Div(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0)).toIntVar
    bench.run
  }

  test("Mod maintains the modulo of two integers") {
    val bench = new InvariantTestBench
    new Mod(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0)).toIntVar
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

  test("Union") {
    val bench = new InvariantTestBench(2)
    new Union(bench.genIntSetVar(0 to 100, 5), bench.genIntSetVar(0 to 100, 5)).toIntSetVar
    bench.run
  }

  test("Inter") {
    val bench = new InvariantTestBench(2)
    new Inter(bench.genIntSetVar(0 to 100, 5), bench.genIntSetVar(0 to 100, 5)).toIntSetVar
    bench.run
  }

  test("Diff") {
    val bench = new InvariantTestBench(2)
    new Diff(bench.genIntSetVar(0 to 100, 5), bench.genIntSetVar(0 to 100, 5)).toIntSetVar
    bench.run
  }

  test("Cardinality") {
    val bench = new InvariantTestBench(2)
    new Cardinality(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }

  test("MakeSet") {
    val bench = new InvariantTestBench(2)
    new MakeSet(bench.genSortedIntVars(10, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Interval") {
    val bench = new InvariantTestBench(2)
    new Interval(bench.genIntVar(0 to 10), bench.genIntVar(90 to 100)).toIntSetVar
    bench.run
  }

  test("TakeAny") {
    val bench = new InvariantTestBench(2)
    new TakeAny(bench.genIntSetVar(0 to 100, 5), Gen.choose(0, 100).sample.get).toIntVar
    bench.run
  }

  test("SetSum") {
    val bench = new InvariantTestBench(2)
    new SetSum(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }

  test("SetProd") {
    val bench = new InvariantTestBench(2)
    new SetProd(bench.genIntSetVar(0 to 100, 5)).toIntVar
    bench.run
  }
}

abstract class Move

case class PlusOne extends Move
case class MinusOne extends Move
case class ToZero extends Move
case class ToMin extends Move
case class ToMax extends Move
case class Random extends Move
case class RandomDiff extends Move

object InvGen {
  val move = Gen.oneOf(PlusOne(), MinusOne(), ToZero(), ToMin(), ToMax(), Random(), RandomDiff())

  def randomIntVar(range: Range, model: Model, constraint: Int => Boolean) = for {
    v <- Gen.choose(range.min, range.max) suchThat (constraint(_))
    c <- Gen.alphaChar
  } yield new RandomIntVar(new IntVar(model, range, v, c.toString), constraint)

  def randomIntVars(nbVars: Int, range: Range, model: Model, constraint: Int => Boolean) = {
    Gen.containerOfN[List, RandomIntVar](nbVars, randomIntVar(range, model, constraint))
  }

  def randomFixedIntSetVar(range: Range, size: Int, model: Model) = for {
    c <- Gen.alphaChar
    v <- Gen.containerOfN[List, Int](size, Gen.choose(range.min, range.max))
  } yield new RandomIntSetVar(new IntSetVar(model, range.min, range.max, c.toString, SortedSet(v: _*)))

  def randomIntSetVar(range: Range, upToSize: Int, model: Model) = for {
    c <- Gen.alphaChar
    s <- Gen.choose(1, upToSize)
    v <- Gen.containerOfN[List, Int](s, Gen.choose(range.min, range.max))
  } yield new RandomIntSetVar(new IntSetVar(model, range.min, range.max, c.toString, SortedSet(v: _*)))

  def randomIntSetVars(nbVars: Int, upToSize: Int, range: Range, model: Model) = {
    Gen.containerOfN[List, RandomIntSetVar](nbVars, randomIntSetVar(range, upToSize, model))
  }
}

abstract class RandomVar {
  def randomVar(): Variable

  def move(move: Move)

  override def toString = randomVar.toString
}

case class RandomIntVar(intVar: IntVar,
  constraint: Int => Boolean = (v: Int) => true) extends RandomVar {

  override def randomVar(): IntVar = intVar

  def applyConstraint(newVal: Int) {
    if (constraint(newVal)) {
      randomVar := newVal
    }
  }

  override def move(move: Move) = {
    move match {
      case PlusOne() => {
        val newVal = randomVar.value + 1
        if (randomVar.domain.contains(newVal)) applyConstraint(newVal)
        else applyConstraint(randomVar.minVal)
      }
      case MinusOne() => {
        val newVal = randomVar.value - 1
        if (randomVar.domain.contains(newVal)) applyConstraint(newVal)
        else applyConstraint(randomVar.maxVal)
      }
      case ToZero() => {
        applyConstraint(0)
      }
      case ToMax() => {
        applyConstraint(randomVar.maxVal)
      }
      case ToMin() => {
        applyConstraint(randomVar.minVal)
      }
      case Random() => {
        applyConstraint(Gen.choose(randomVar.minVal, randomVar.maxVal).sample.get)
      }
      case RandomDiff() => {
        val randomOpt = (Gen.choose(randomVar.minVal, randomVar.maxVal)
          suchThat (_ != randomVar.value)).sample
        if (randomOpt.isDefined) applyConstraint(randomOpt.get)
      }
    }
  }
}

case class RandomIntSetVar(intSetVar: IntSetVar) extends RandomVar {
  override def randomVar(): IntSetVar = intSetVar

  override def move(move: Move) = {
    move match {
      case PlusOne() => { // Adds an element to the set
        randomVar :+= Gen.choose(randomVar.getMinVal, randomVar.getMaxVal).sample.get
      }
      case MinusOne() => { // Removes an element from the set
        if (!randomVar.value.isEmpty) randomVar :-= Gen.oneOf(randomVar.value.toSeq).sample.get
        //else randomVar.value = Seq.fill(randomVar.value.size)(util.Random.nextInt)
      }
      case ToZero() => { // Removes all elements from the set
        randomVar.value.foreach(value => randomVar.deleteValue(value))
      }
      case ToMax() => { // Adds all elements between min and max to the set
        (randomVar.getMinVal to randomVar.getMaxVal).foreach(v => randomVar :+= v)
      }
      case ToMin() => { // Reduces the set to a singleton
        randomVar.value.foreach(value => randomVar.deleteValue(value))
        randomVar :+= Gen.choose(randomVar.getMinVal, randomVar.getMaxVal).sample.get
      }
      case Random() => { // Replaces the set with a randomly generated one
        val newSize = Gen.choose(1, randomVar.value.size + 1).sample.get
        val newVal = Gen.containerOfN[List, Int](newSize,
          Gen.choose(randomVar.getMinVal, randomVar.getMaxVal)).sample.get
        randomVar := SortedSet(newVal: _*)
      }
      case RandomDiff() => {
        // Replaces the set with a randomly generated one
        // with which intersection is empty
        val newSize = Gen.choose(1, randomVar.value.size + 1).sample.get
        val newValOpt = Gen.containerOfN[List, Int](newSize,
          Gen.choose(randomVar.getMinVal, randomVar.getMaxVal)
            suchThat (!randomVar.value.contains(_))).sample
        if (newValOpt.isDefined) randomVar := SortedSet(newValOpt.get: _*)
      }
    }
  }
}

class InvariantTestBench(verbose: Int = 0) {
  var property: Prop = false
  val checker = new InvariantChecker(verbose)
  val model = new Model(false, Some(checker))
  var inputVars: List[RandomVar] = List()
  var outputVars: List[RandomVar] = List()

  def addVar(input: Boolean, v: RandomVar) {
    addVar(input, List(v))
  }

  def addVar(input: Boolean, vars: Iterable[RandomVar]) {
    for (v <- vars) {
      if (input) inputVars = v :: inputVars
      else outputVars = v :: outputVars
    }
  }

  def genIntVar(
    range: Range,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): IntVar = {
    val riVar = InvGen.randomIntVar(range, model, constraint).sample.get
    addVar(isInput, riVar)
    riVar.randomVar
  }

  def genIntVarsArray(
    nbVars: Int = 4,
    range: Range = 0 to 100,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): Array[IntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model, constraint).sample.get
    addVar(isInput, riVars)
    riVars.map((riv: RandomIntVar) => {
      riv.randomVar
    }).toArray
  }

  implicit val intVarOrdering: Ordering[IntVar] = Ordering.by(_.value)

  def genSortedIntVars(
    nbVars: Int,
    range: Range,
    isInput: Boolean = true,
    constraint: Int => Boolean = (v: Int) => true): SortedSet[IntVar] = {
    val riVars = InvGen.randomIntVars(nbVars, range, model, constraint).sample.get
    addVar(isInput, riVars)
    val iVars = riVars.map((riv: RandomIntVar) => { riv.randomVar })
    SortedSet(iVars: _*)(intVarOrdering)
  }

  def genIntSetVar(range: Range, size: Int, isInput: Boolean = true) = {
    val risVar = InvGen.randomFixedIntSetVar(range, size, model).sample.get
    addVar(isInput, risVar)
    risVar.randomVar
  }

  def genIntSetVars(nbVars: Int = 4, upToSize: Int = 20, range: Range = 0 to 100, isInput: Boolean = true): Array[IntSetVar] = {
    val risVars = InvGen.randomIntSetVars(nbVars, upToSize, range, model).sample.get
    addVar(isInput, risVars)
    risVars.map((risv: RandomIntSetVar) => {
      risv.randomVar
    }).toArray
  }

  def printVars(name: String, vars: List[RandomVar]) {
    if (vars.length > 0) {
      println(name + " vars:")
      vars.foreach((rv: RandomVar) => println(rv.toString()))
      println
    }
  }

  def run() = {
    model.close()
    //println("Model closed")
    model.propagate()

    try {
      property = org.scalacheck.Prop.forAll(InvGen.move) {
        randomMove: Move =>
          if (verbose > 0) {
            println("---------------------------------------------------")
            printVars("Input", inputVars)
            printVars("Output", outputVars)
            print(randomMove.toString() + " ")
          }
          val randomVar = Gen.oneOf(inputVars).sample.get
          if (verbose > 0) print(randomVar.toString() + " => ")
          randomVar.move(randomMove)
          if (verbose > 0) println(randomVar.toString() + "\n")
          model.propagate()
          if (verbose > 0) println
          checker.isChecked
      }
    } catch {
      case e: Exception =>
        println("Exception caught: " + e)
    }
    Checkers.check(property)
  }
}
