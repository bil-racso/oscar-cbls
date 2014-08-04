package oscar.algo.reversible

/**
 * Generic Reversible inside a reversible node
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class Reversible[T](val context: ReversibleContext) {
  
  private var lastMagic: Long = -1L

  @inline protected final def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {   
      lastMagic = contextMagic
      context.pushOnTrail(this, value)
    }
  }
  
  /** Return the current value of the reversible */
  def value: T
  
  /** Restores the state of the object */
  def restore(value: T): Unit  
}