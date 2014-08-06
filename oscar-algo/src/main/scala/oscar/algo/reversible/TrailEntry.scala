package oscar.algo.reversible

abstract class TrailEntry { 
  def restore(): Unit
  def magic: Long
}

class TrailEntryImpl[@specialized T](reversible: Reversible[T], value: T, override val magic: Long) extends TrailEntry {
  @inline override final def restore(): Unit = reversible.restore(value)
}
