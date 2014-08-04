package oscar.algo.reversible

abstract class TrailEntry { def restore(): Unit }

class TrailEntryImpl[@specialized T](val reversible: Reversible[T], var value: T) extends TrailEntry {
  @inline override final def restore(): Unit = reversible.restore(value)
}
