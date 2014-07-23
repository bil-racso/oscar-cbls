/*******************************************************************************
" * OscaR is free software: you can redistribute it and/or modify
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
 ******************************************************************************/

package oscar.algo.reversible

import java.util.Stack
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

/**
 * Class representing a reversible node, that is a node able to restore all
 * the reversible state attached to it (see Reversibles). <br>
 * @author Pierre Schaus pschaus@gmail.com
 */
class ReversibleContext {

  protected var magicNumber: Int = 0
  protected var trailStack: Trail = new Trail()

  private val pointerStack = new Stack[TrailEntry]()
  
  // Actions to execute when a pop operation occurs 
  private val popListeners = new ArrayBuffer[() => Unit]()
  
  /** Returns the magic number of the context */
  def magic: Int = magicNumber
  
  /** Returns the stack of trails */
  def trail: Trail = trailStack
  
  
  /** Adds an action to execute when the `pop` function us called */
  def onPop(action: => Unit): Unit = popListeners.add(() => action)
  
  /** Stores the current state of the node on a stack */
  def pushState(): Unit = {
    magicNumber += 1
    pointerStack.push(trail.getTopEntry)
  }

  /** Restores state on top of the stack of states and remove it from the stack */
  def pop(): Unit = {
    trail.restoreUntil(pointerStack.pop())
    popListeners.foreach(_())
    magicNumber += 1 // increment the magic because we want to trail again
  }

  /** 
   *  Restores the node to its initial state 
   *  Note: does not execute the on pop actions
   */
  def popAll(): Unit = {
    while (!pointerStack.empty()) {
      trail.restoreUntil(pointerStack.pop())
    }
    magicNumber += 1 // increment the magic because we want to trail again
  }

  override def toString: String = "SearchNode nPushed: " + pointerStack.size + " currentTrailSize: " + trail.getSize
}
