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

  private var maxTrailSize: Int = 0
  private var trailTime: Long = 0
  private var magicNumber: Long = 0

  import oscar.algo.ArrayStack // custom version of ArrayStack
  private val trailStack: ArrayStack[TrailEntry] = new ArrayStack(1000)
  private val pointerStack: ArrayStack[TrailEntry] = new ArrayStack(100)

  // Used to reference the initial state
  trailStack.push(null)

  // Actions to execute when a pop occurs 
  private val popListeners = new ArrayBuffer[() => Unit]()

  /** Returns the magic number of the context */
  final def magic: Long = magicNumber

  /** Returns the maximal size of the trailing stack */
  final def maxSize: Int = maxTrailSize

  /** Returns the time spent to pop states */
  final def time: Long = trailTime

  /** Adds an action to execute when the `pop` function is called */
  def onPop(action: => Unit): Unit = popListeners.append(() => action)
  
  def trail(entry: TrailEntry): Unit = {
    trailStack.push(entry)
    val size = trailStack.size
    if (size > maxTrailSize) maxTrailSize = size
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
    popListeners.foreach(_())
    // Increments the magic because we want to trail again
    magicNumber += 1
  }

  @inline private def restoreUntil(until: TrailEntry): Unit = {
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
