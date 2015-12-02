package oscar.cbls.invariants.lib.minmax

import oscar.cbls.invariants.core.algo.heap.{ArrayMap, BinomialHeapWithMoveExtMem}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.KeyForElementRemoval

/**
 * Maintains Miax(Var(i) | i in cond)
 * Exact ordering is specified by implementing abstract methods of the class.
 * @param vars is an array of IntVar, which can be bulked
 * @param cond is the condition, can be null
 * update is O(log(n))
 * @author renaud.delandtsheer@cetic.be
 * */
abstract class MiaxArrayLazy(vars: Array[IntValue], cond: SetValue, default: Int)
  extends IntInvariant with Bulked[IntValue, Domain] with VaryingDependencies {

  var keyForRemoval: Array[KeyForElementRemoval] = new Array(vars.length)
  var h: BinomialHeapWithMoveExtMem[Int] = new BinomialHeapWithMoveExtMem[Int](i => Ord(vars(i)), vars.length, new ArrayMap(vars.length))

  if (cond != null) {
    registerStaticDependency(cond)
    registerDeterminingDependency(cond)
  }

  /*
   * six internal state of a value:
   * out
   * out and not removed yet
   * in, up to date, impacting min-max
   * in, not up to date, impacting min-max
   * in, not up to date, impacting min-max
   */
  /**
   * since the value of the bulkcomputationResult depends on the presence or absence of cond,
   * we register two bcr, so that you can join the correct bulk whataver happens.
   */
  restrictDomain(bulkRegister(vars,if (cond == null) 0 else 1).union(default))

  if (cond != null) {
    for (i <- cond.value) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  } else {
    for (i <- vars.indices) {
      h.insert(i)
      keyForRemoval(i) = registerDynamicDependency(vars(i), i)
    }
  }

  finishInitialization()

  override def performBulkComputation(bulkedVar: Array[IntValue]) =
    InvariantHelper.getMinMaxBounds(bulkedVar)

  def ExtremumName: String
  def Ord(v: IntValue): Int

  if (h.isEmpty) {
    this := default
  } else {
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyIntChanged(v: ChangingIntValue, index: Int, OldVal: Int, NewVal: Int) {
    //mettre a jour le heap
    h.notifyChange(index)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyInsertOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)
    keyForRemoval(value) = registerDynamicDependency(vars(value), value)

    //mettre a jour le heap
    h.insert(value)
    this := vars(h.getFirst).value
  }

  @inline
  override def notifyDeleteOn(v: ChangingSetValue, value: Int) {
    assert(v == cond)

    keyForRemoval(value).performRemove()
    keyForRemoval(value) = null

    //mettre a jour le heap
    h.delete(value)
    if (h.isEmpty) {
      this := default
    } else {
      this := vars(h.getFirst).value
    }
  }
}
