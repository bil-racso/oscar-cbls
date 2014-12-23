/**
 * *****************************************************************************
 * " * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */

package oscar.algo.reversible

import scala.collection.mutable.ArrayBuffer

/**
 * Class representing a reversible node, that is a node able to restore all
 * the reversible state attached to it (see Reversibles). <br>
 *
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class ReversibleContext {

  private[this] var maxTrailSize: Int = 0
  private[this] var trailTime: Long = 0
  private[this] var magicNumber: Long = 0

  import oscar.algo.ArrayStack // custom version of ArrayStack
  private[this] val trailStack: ArrayStack[TrailEntry] = new ArrayStack(1000)
  private[this] val pointerStack: ArrayStack[TrailEntry] = new ArrayStack(100)
  
  // Actions to execute when a pop occurs 
  private[this] val popListeners = new ArrayStack[() => Unit](4)

  // Used to reference the initial state
  trailStack.push(null)
  
  /** Returns the magic number of the context */
  final def magic: Long = magicNumber

  /** Returns the maximal size of the trailing stack */
  final def maxSize: Int = maxTrailSize

  /** Returns the time spent to pop states */
  final def time: Long = trailTime

  /** Adds an action to execute when the `pop` function is called */
  def onPop(action: => Unit): Unit = popListeners.push(() => action)
  
  /** Trail the entry such that its restore method is called on corresponding pop */
  @inline final def trail(entry: TrailEntry): Unit = {
    trailStack.push(entry)
    val size = trailStack.size
    if (size > maxTrailSize) maxTrailSize = size
  }
  
  /** Trail the closure such that it is called on corresponding pop */
  final def trail[@specialized T](closure: => T): Unit = {
    trail(new TrailEntry { final override def restore(): Unit = closure })
  }

  /** Stores the current state of the node on a stack */
  def pushState(): Unit = {
    magicNumber += 1
    pointerStack.push(trailStack.top)
  }

  /** Restores state on top of the stack of states and remove it from the stack */
  def pop(): Unit = {
    // Restores the state of each reversible
    restoreUntil(pointerStack.pop())
    // Executes onPop actions
    popListeners.foreach(action => action())
    // Increments the magic because we want to trail again
    magicNumber += 1
  }

  @inline private final def restoreUntil(until: TrailEntry): Unit = {
    val t0 = System.currentTimeMillis()
    while (trailStack.top != until) {
      val entry = trailStack.pop()
      entry.restore()
    }
    trailTime += System.currentTimeMillis() - t0
  }

  /**
   *  Restores the node to its initial state
   *  Note: does not execute the on pop actions
   */
  def popAll(): Unit = {
    if (!pointerStack.isEmpty) {
      restoreUntil(pointerStack.last)
    }
    // Increments the magic because we want to trail again
    magicNumber += 1
  }
  
  /** Empty the trailing queue without restoring trailed objects */
  final def clear(): Unit = {
    trailStack.clear()   // does not remove references
    pointerStack.clear() // does not remove references
  }

  /**
   *  
   * 
   */
  def clear(): Unit = {
    trailStack.clearRefs()
    pointerStack.clearRefs()
  }
  
  def resetStats(): Unit = {
    trailTime = 0
    maxTrailSize = 0
  }

  override def toString: String = "SearchNode nPushed: " + pointerStack.size + " currentTrailSize: " + trailStack.size
}
