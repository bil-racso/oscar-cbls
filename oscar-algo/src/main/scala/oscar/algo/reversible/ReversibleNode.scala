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

  var magic = 0
  var trail = new Trail();
  val pointerStack = new Stack[TrailEntry]()

  
  val popListeners = new ArrayBuffer[() => Unit]()
  
  def onPop(action: => Unit) {
    popListeners.add(() => action)
  }  

  def getMagic() = magic

  def getTrail() = trail


  /**
   * store the current state of the node on a stack.
   */
  def pushState() {
    magic += 1
    pointerStack.push(trail.getTopEntry())
  }

  /**
   * Restore state on top of the stack of states and remove it from the stack.
   */
  def pop() {
    trail.restoreUntil(pointerStack.pop())
    popListeners.foreach(_())
    magic += 1 // increment the magic because we want to trail again
  }

  /**
   * Restore the node to its initial state
   */
  def popAll() {
    while (!pointerStack.empty()) {
      trail.restoreUntil(pointerStack.pop())
    }
    magic += 1 // increment the magic because we want to trail again
  }

  override def toString() = {
    "SearchNode: nbPushed" + pointerStack.size() + " currentTrailSize:" + trail.getSize();
  }

}
