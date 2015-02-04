package oscar.algo.search


import scala.util.Random
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.ReversibleBoolean

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class DFSearchNode extends ReversibleContext {

  var silent = false
  
  private[this] val failed = new ReversibleBoolean(this, false)

  /** @return  true if this node can surely not lead to any solution */
  def isFailed(): Boolean = failed.value

  /** Set the node in a failed state */
  def fail(): Unit = failed.setTrue()
  
  /** This function is executed when the node becomes a solution */
  def solFound(): Unit = Unit
}