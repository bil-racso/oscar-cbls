package oscar.cbls.test.invariants

import scala.collection.immutable.SortedSet
import collection.immutable.SortedMap
import org.scalacheck.Gen
import org.scalacheck.Gen.value
import org.scalacheck.Prop
import org.scalatest.prop.Checkers
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic._
import org.scalatest.FunSuite
import oscar.cbls.invariants.lib.numeric.Mod
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.invariants.lib.minmax.MinLin
import oscar.cbls.invariants.lib.set.MakeSet
import oscar.cbls.invariants.lib.minmax.MinSet
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.invariants.lib.set.Inter
import oscar.cbls.invariants.lib.set.SetSum
import oscar.cbls.constraints.lib.global.Sequence
import oscar.cbls.invariants.lib.logic.IntITE
import oscar.cbls.invariants.lib.numeric.Prod
import scala.Some
import oscar.cbls.invariants.lib.set.Interval
import oscar.cbls.invariants.lib.minmax.MinArray
import oscar.cbls.invariants.lib.set.SetProd
import oscar.cbls.invariants.lib.minmax.ArgMinArray
import oscar.cbls.invariants.lib.minmax.MaxLin
import oscar.cbls.invariants.lib.numeric.ProdElements
import oscar.cbls.invariants.lib.minmax.Min2
import oscar.cbls.invariants.lib.numeric.Prod2
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.invariants.lib.logic.SelectLEHeapHeap
import oscar.cbls.invariants.lib.minmax.ArgMaxArray
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.numeric.Minus
import oscar.cbls.constraints.lib.global.MultiKnapsack
import oscar.cbls.invariants.lib.set.Cardinality
import oscar.cbls.constraints.lib.global.AtMost
import oscar.cbls.invariants.lib.numeric.Abs
import oscar.cbls.invariants.lib.minmax.Max
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.numeric.Step
import oscar.cbls.invariants.lib.logic.IntSetElement
import oscar.cbls.invariants.lib.logic.IntElements
import oscar.cbls.invariants.lib.numeric.Div
import oscar.cbls.invariants.lib.set.Union
import oscar.cbls.invariants.lib.minmax.Min
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.invariants.lib.logic.DenseCount
import oscar.cbls.constraints.lib.global.AtLeast
import oscar.cbls.invariants.lib.set.Diff
import oscar.cbls.invariants.lib.set.TakeAny
import oscar.cbls.invariants.lib.minmax.MaxArray
import oscar.cbls.invariants.lib.minmax.MaxSet
import oscar.cbls.invariants.lib.logic.SelectLESetQueue
import oscar.cbls.invariants.lib.numeric.RoundUpModulo
import oscar.cbls.constraints.lib.basic.BelongsTo
import oscar.cbls.constraints.lib.basic.LE
import oscar.cbls.constraints.lib.basic.GE
import oscar.cbls.constraints.lib.basic.G
import oscar.cbls.constraints.lib.basic.L
import oscar.cbls.constraints.lib.basic.NE
import oscar.cbls.constraints.lib.basic.EQ
import oscar.cbls.test.invariants.bench.{InvGen, InvBench, InvariantChecker}

class smalltest extends FunSuite with Checkers {
  //this is not working so far.
  test("SelectLEHeapHeap") {
    val bench = new InvBench(2)
    new SelectLEHeapHeap(bench.genIntVarsArray(4, 0 to 5), bench.genIntVar(3 to 10)).toIntSetVar
    bench.run
  }
}

class InvariantTests extends FunSuite with Checkers {

  val verbose = 0



  test("BelongsTo maintains the violation of a membership.") {
    val bench = new InvBench(verbose)
    new BelongsTo(bench.genIntVar(0 to 10), bench.genIntSetVar(5, 0 to 10)).toIntVar
    bench.run
  }

  test("LE maintains the violation of a lesser or equal test.") {
    val bench = new InvBench(verbose)
    new LE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("GE maintains the violation of a greater or equal test.") {
    val bench = new InvBench(verbose)
    new GE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("L maintains the violation of a strict lesser test.") {
    val bench = new InvBench(verbose)
    new L(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("G maintains the violation of a strict greater test.") {
    val bench = new InvBench(verbose)
    new G(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("NE maintains the violation of a inequality test.") {
    val bench = new InvBench(verbose)
    new NE(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("EQ maintains the violation of an equality test.") {
    val bench = new InvBench(verbose)
    new EQ(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("AllDiff maintains output = all int vars have different values") {
    val bench = new InvBench(verbose)
    new AllDiff(bench.genIntVarsArray(10, -10 to 10))
    bench.run
  }

  test("AtLeast") {
    val bench = new InvBench(verbose)
    new AtLeast(bench.genIntVars(10), bench.genBoundedValues(10, 0 to 30, 0 to 10)).toIntVar
    bench.run
  }

  test("AtMost") {
    val bench = new InvBench(verbose)
    new AtMost(bench.genIntVars(10), InvGen.randomIntSortedMap(10, 0 to 30, 0 to 30)).toIntVar
    bench.run
  }

  test("MultiKnapsack") {
    val bench = new InvBench(verbose)
    new MultiKnapsack(
      bench.genIntVarsArray(10, 0 to 5),
      bench.genIntVarsArray(10, 2 to 7),
      bench.genIntVarsArray(6, 1 to 10)).toIntVar
    bench.run
  }

  test("Sequence") {
    val bench = new InvBench(verbose)
    new Sequence(
      bench.genIntVarsArray(),
      Gen.choose(10, 20).sample.get,
      Gen.choose(2, 9).sample.get,
      (x: Int) => x > 1).toIntVar
    bench.run
  }

  test("Access to ITE maintains output = if ifVar > 0 then thenVar else elseVar") {
    val bench = new InvBench(verbose)
    new IntITE(
      bench.genIntVar(-2 to 3),
      bench.genIntVar(1 to 2),
      bench.genIntVar(10 to 11)).toIntVar
    bench.run
  }

  test("Access to int element maintains output = array(index)") {
    val bench = new InvBench(verbose)
    new IntElement(
      bench.genIntVar(0 to 19),
      bench.genIntVarsArray(20, 0 to 100)).toIntVar
    bench.run
  }

  test("Access to int vars...") {
    val bench = new InvBench(verbose)
    new IntElements(
      bench.genIntSetVar(3, 0 to 4),
      bench.genIntVarsArray(5, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Access to int set element maintains output = array(index)") {
    val bench = new InvBench(verbose)
    new IntSetElement(
      bench.genIntVar(0 to 19),
      bench.genIntSetVars(20, 10, 0 to 100)).toIntSetVar
    bench.run
  }

  test("Sparse Cluster maintains a cluster of the indexes of an array.") {
    val bench = new InvBench(verbose)
    Cluster.MakeSparse(bench.genIntVarsArray(50),
      (Gen.containerOfN[List, Int](100, Gen.choose(0, 100))).sample.get)
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array.") {
    val bench = new InvBench(verbose)
    Cluster.MakeDense(bench.genIntVarsArray(50))
    bench.run
  }

  test("Dense Cluster maintains a cluster of all the indexes of an array"
    + " (assuming min and max).") {
    val bench = new InvBench(verbose)
    Cluster.MakeDenseAssumingMinMax(bench.genIntVarsArray(50), 0, 100)
    bench.run
  }

  test("Dense Count maintains count(j) = #{i in index of values | values[i] == j}") {
    val bench = new InvBench(verbose)
    new DenseCount(
      bench.genIntVarsArray(10, 0 to 19),
      bench.genIntVarsArray(20, 0 to 19, false))
    bench.run
  }

  ignore("Cross references...")(pending)

  ignore("Cumulative...")(pending)

  test("Filter...") {
    val bench = new InvBench(verbose)
    new Filter(
      bench.genIntVarsArray(4, 0 to 5),
      (i: Int) => (i % 2) == 0).toIntSetVar
    bench.run
  }

  test("SelectLEHeapHeap") {
    val bench = new InvBench(verbose)
    new SelectLEHeapHeap(
      bench.genIntVarsArray(4, 0 to 5),
      bench.genIntVar(3 to 10)).toIntSetVar
    bench.run
  }

  ignore("SelectLESetQueue") {
    //TODO exclure les changements de valeurs interdits
    //le pivot ne peut qu'augmenter
    //une valeur en dessous du pivot ne peut que prendre une valeur dépassant toutes les autres valeurs
    //les valeurs au dessus du pivot ne peuvent pas changer
    val bench = new InvBench(verbose)
    new SelectLESetQueue(bench.genIntVarsArray(5, 0 to 5), bench.genIntVar(3 to 10, false)).toIntSetVar
    bench.run
  }

  ignore("Predecessor")(pending)

  ignore("Routes")(pending)

  //this is not working so far.
  test("Sort") {
    val bench = new InvBench(verbose)
    Sort.MakeSort(bench.genIntVarsArray(4, 0 to 30))
    bench.run
  }

  // TODO test also with the other parameters of ArgMinArray
  test("ArgMinArray maintains the set of min variables of the array") {
    val bench = new InvBench(verbose)
    new ArgMinArray(bench.genIntVarsArray(20, 0 to 30)).toIntSetVar
    bench.run
  }

  // TODO test also with the other parameters of ArgMaxArray
  test("ArgMaxArray maintains the set of max variables of the array") {
    val bench = new InvBench(verbose)
    new ArgMaxArray(bench.genIntVarsArray(20, 0 to 30)).toIntSetVar
    bench.run
  }

  test("MaxLin") {
    val bench = new InvBench(verbose)
    new MaxLin(bench.genSortedIntVars(6, -10 to 10)).toIntVar
    bench.run
  }

  test("MinLin") {
    val bench = new InvBench(verbose)
    new MinLin(bench.genSortedIntVars(6, 0 to 10)).toIntVar
    bench.run
  }

  test("Min") {
    val bench = new InvBench(verbose)
    new Min(bench.genSortedIntVars(5, -10 to 10)).toIntVar
    bench.run
  }

  test("Max") {
    val bench = new InvBench(verbose)
    new Max(bench.genSortedIntVars(5, -10 to 10)).toIntVar
    bench.run
  }

  test("Min2") {
    val bench = new InvBench(verbose)
    new Min2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("Max2") {
    val bench = new InvBench(verbose)
    new Max2(bench.genIntVar(-10 to 10), bench.genIntVar(-10 to 10)).toIntVar
    bench.run
  }

  test("MinArray maintains the minimum from an array of variables.") {
    val bench = new InvBench(verbose)
    new MinArray(bench.genIntVarsArray(4, 0 to 100)).toIntVar
    bench.run
  }

  test("MaxArray maintains the maximum from an array of variables.") {
    val bench = new InvBench(verbose)
    new MaxArray(bench.genIntVarsArray(2, 0 to 50)).toIntVar
    bench.run
  }

  test("MinSet maintains the minimum of a set.") {
    val bench = new InvBench(verbose)
    new MinSet(bench.genIntSetVar()).toIntVar
    bench.run
  }

  test("MaxSet maintains the maximum of a set") {
    val bench = new InvBench(verbose)
    new MaxSet(bench.genIntSetVar()).toIntVar
    bench.run
  }

  test("SumElements maintains the sum of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose)
    new SumElements(
      bench.genIntVarsArray(10, 0 to 100),
      bench.genIntSetVar(5, 0 to 9)).toIntVar
    bench.run
  }

  test("ProdElements maintains the product of variables of which indices are in the given set.") {
    val bench = new InvBench(verbose)
    new ProdElements(
      bench.genIntVarsArray(10, 0 to 10),
      bench.genIntSetVar(2, 0 to 9)).toIntVar
    bench.run
  }

  test("Sum maintains the sum of input variables.") {
    val bench = new InvBench(verbose)
    new Sum(bench.genIntVarsArray(10, 0 to 100)).toIntVar
    bench.run
  }

  test("Prod maintains the product of input variables.") {
    val bench = new InvBench(verbose)
    new Prod(bench.genIntVarsArray(3, 0 to 100)).toIntVar
    bench.run
  }

  test("Minus maintains the difference between two variables.") {
    val bench = new InvBench(verbose)
    new Minus(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Sum2 maintains the sum of two variables.") {
    val bench = new InvBench(verbose)
    new Sum2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Prod2 maintains the product of two variables") {
    val bench = new InvBench(verbose)
    new Prod2(bench.genIntVar(0 to 100), bench.genIntVar(0 to 100)).toIntVar
    bench.run
  }

  test("Div maintains the division of two variables.") {
    val bench = new InvBench(verbose)
    new Div(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0)).toIntVar
    bench.run
  }

  test("Mod maintains the modulo of two variables.") {
    val bench = new InvBench(verbose)
    new Mod(bench.genIntVar(0 to 100),
      bench.genIntVar(1 to 100, true, (v: Int) => v != 0)).toIntVar
    bench.run
  }

  test("Abs maintains the absolute value of a variable.") {
    val bench = new InvBench(verbose)
    new Abs(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  test("Step maintains a step function of the input var.") {
    val bench = new InvBench(verbose)
    new Step(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  test("RoundUpModulo") {
    val bench = new InvBench(verbose)
    new RoundUpModulo(
      bench.genIntVar(0 to 10),
      bench.genIntVar(0 to 10),
      Gen.choose(0, 10).sample.get,
      Gen.choose(0, 10).sample.get,
      Gen.choose(0, 10).sample.get).toIntVar
    bench.run
  }

 /* test("RoundUpCustom") {
    val bench = new InvBench(verbose)
    new RoundUpCustom(
      bench.genIntVar(0 to 10),
      bench.genIntVar(0 to 10),
      InvGen.randomTuples(10, 0 to 10)).toIntVar
    bench.run
  }*/

  test("Union maintains the union of two sets.") {
    val bench = new InvBench(verbose)
    new Union(bench.genIntSetVar(), bench.genIntSetVar()).toIntSetVar
    bench.run
  }

  test("Inter maintains the intersection of two sets.") {
    val bench = new InvBench(verbose)
    new Inter(bench.genIntSetVar(), bench.genIntSetVar()).toIntSetVar
    bench.run
  }

  test("Diff maintains the difference between two sets.") {
    val bench = new InvBench(verbose)
    new Diff(bench.genIntSetVar(), bench.genIntSetVar()).toIntSetVar
    bench.run
  }

  test("Cardinality maintains the cardinality of a set.") {
    val bench = new InvBench(verbose)
    new Cardinality(bench.genIntSetVar()).toIntVar
    bench.run
  }

  test("MakeSet maintains an IntSetVar given a set of IntVar.") {
    val bench = new InvBench(verbose)
    new MakeSet(bench.genSortedIntVars(10, 0 to 10)).toIntSetVar
    bench.run
  }

  test("Interval maintains the set in the interval.") {
    val bench = new InvBench(verbose)
    new Interval(
      bench.genIntVar(-100 to 100),
      bench.genIntVar(-100 to 100)).toIntSetVar
    bench.run
  }

  test("TakeAny maintains a value taken from the set.") {
    val bench = new InvBench(verbose)
    new TakeAny(bench.genIntSetVar(), 0).toIntVar
    bench.run
  }

  test("SetSum maintains the sum of variables (after optionnaly appliying a function).") {
    val bench = new InvBench(verbose)
    new SetSum(bench.genIntSetVar()).toIntVar
    bench.run
  }

  /**
   * Won't pass when the product products an overflow.
   */
  test("SetProd maintains the product of variables (after optionnaly appliying a function).") {
    val bench = new InvBench(verbose)
    new SetProd(bench.genIntSetVar(10, -3 to 3)).toIntVar
    bench.run
  }

  test("IdentityInt maintains the identity of an integer).") {
    val bench = new InvBench(verbose)
    new IdentityInt(bench.genIntVar(-100 to 100)).toIntVar
    bench.run
  }

  test("IdentityIntSet maintains the identity of a set of integers).") {
    val bench = new InvBench(verbose)
    new IdentityIntSet(bench.genIntSetVar()).toIntSetVar
    bench.run
  }
}

