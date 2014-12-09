package oscar.algo.reversible

/**
 * Abstract class for reversible data structure
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class Reversible {
  
  private final var lastMagic: Long = -1L
  
  @inline final protected def trail(): Unit = {
    val contextMagic = context.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      context.trail(trailEntry)
    }
  }
  
  def context: ReversibleContext
  
  def trailEntry: TrailEntry
}

abstract class TrailEntry { 
  def restore(): Unit
}