package oscar.cp.core.domains

import oscar.cp.core.CPStore
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.reversible.ReversibleContext
import oscar.cp.core.CPOutcome
import scala.util.Random

/**
 *  An adaptable sparse integer domain. The structure used to implement the domain
 *  evolves dynamically with the operations applied on the domain.
 *
 *  @author Renaud Hartert
 */
class AdaptableIntDomain(override val context: ReversibleContext, val minValue: Int, val maxValue: Int) extends IntDomain {

  // Reversible domain representation
  private val domain: ReversiblePointer[IntervalDomain] = {
    new ReversiblePointer[IntervalDomain](context, new BoundDomain(context, minValue, maxValue))
  }

  override def size: Int = domain.value.size

  override def isEmpty: Boolean = domain.value.isEmpty

  override def isBound: Boolean = domain.value.isBound

  override def max: Int = domain.value.max

  override def min: Int = domain.value.min

  override def randomValue(rand: Random): Int = domain.value.randomValue(rand)

  override def hasValue(value: Int): Boolean = domain.value.hasValue(value)

  override def removeValue(value: Int): CPOutcome = {
    val dom = domain.value
    // If sparse representation, remove
    if (dom.isInstanceOf[IntDomain]) {
      dom.asInstanceOf[IntDomain].removeValue(value)
    } else {
      // UpdateMin if possible
      val minValue = dom.min
      if (minValue == value) dom.updateMin(value + 1)
      else {
        // UpdateMax if possible
        val maxValue = dom.max
        if (maxValue == value) dom.updateMax(value - 1)
        else {
          // Dynamically change the representation of the domain
          // - Use a bit vector for a small and dense domain (not yet available)
          // - Otherwise, use a sparse set
          //if (maxValue - minValue >= 8) { // 8 has been chosen arbitrarily
            val sparse = new SparseSetDomain(domain.node, minValue, maxValue)
            domain.value = sparse
            sparse.removeValue(value)
          /*} else {
            val sparse = new SingleBitVectorDomain(domain.node, minValue, maxValue)
            domain.value = sparse
            sparse.removeValue(value)
          }*/
        }
      }
    }
  }

  override def assign(value: Int): CPOutcome = domain.value.assign(value)

  override def updateMin(value: Int): CPOutcome = domain.value.updateMin(value)

  override def updateMax(value: Int): CPOutcome = domain.value.updateMax(value)

  override def nextValue(value: Int): Int = domain.value.nextValue(value)

  override def prevValue(value: Int): Int = domain.value.prevValue(value)

  override def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = domain.value.delta(oldMin, oldMax, oldSize)

  override def iterator: Iterator[Int] = domain.value.iterator

  override def toString: String = domain.value.toString
}