package oscar.algo.reversible

/**
 * Creates a generic reversible pointer
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class Reversible[@specialized T](val context: ReversibleContext, initialValue: T) {
  
  private var lastMagic: Long = -1L
  
  // Reference on the current value
  protected var pointer: T = initialValue
  
  @inline final def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      val entry = new ReversiblePointerTrailEntry[T](this, pointer)
      context.trail(entry)
    }
  }

  @inline final def setValue(value: T): Unit = {
    if (value != pointer) {
      trail()
      this.pointer = value
    }
  }

  /** @param value to assign */
  @inline final def value_= (value: T): Unit = setValue(value)
  
  /** @param value to assign */
  final def := (value: T): Unit = setValue(value)
  
  /** @return current value */
  @inline final def value = pointer

  /**
   * Check if the pointer is different from null
   * @return true if the pointer is != null, false otherwise
   */
  @inline final def hasValue(): Boolean = pointer != null

  /** @return the current pointer */
  @inline final def getValue(): T = pointer

  @inline final def restore(value: T): Unit = pointer = value

  override def toString(): String = if (hasValue) pointer.toString else ""
}

object Reversible {
  def apply[T](node: ReversibleContext, value: T): Reversible[T] = {
    new Reversible[T](node, value)
  }
}
