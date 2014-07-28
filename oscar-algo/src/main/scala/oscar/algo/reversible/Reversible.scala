package oscar.algo.reversible

/**
 * Generic Reversible inside a reversible node
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class Reversible[T](val node: ReversibleContext) {
  
  private var lastMagic: Long = -1L
  
  /** Returns true if the magic number is not the same as the magic number of the context */
  protected def mustBeTrailed: Boolean = lastMagic != node.magic

  @inline protected def trail(): Unit = {
    val contextMagic = node.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      addOnTrail()
    }
  }
  
  /** Adds what encapsulates the state of the object on the trail */
  protected def addOnTrail(): Unit
  
  /** Restores the state of the object */
  def restore(value: T): Unit  
}