/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.search


import scala.util.continuations._
import oscar.reversible.ReversibleSearchNode
import scala.collection.mutable.Stack



class MyContinuation(msg: String, block: => Unit)  {
      def call() = {
        block
      }
      override def toString = msg
}

/**
 * Non deterministic search controller as described in the wonderful paper:
 * "Non Deterministic Control for Hybrid Search" by Pascal Van Hentenryck and Laurent Michel.
 * In their paper they introduce the nicest possible way to implement a search for CP (in Comet).
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class SearchController(val node: ReversibleSearchNode) {
  
  
  
  var nbFail = 0
  
  private var nFailLimit = Int.MaxValue
  
  /**
   * set the failure limit for this controller
   */
  def failLimit_= (limit: Int) {
    nFailLimit = limit
  }
  
  var limitReached = false
  
  var exit = false
  
  def stop() {
    exit = true
  }
  
  def addChoice(e: MyContinuation)
  
  def start(): Unit@suspendable = {}
  
  def fail() { 
      nbFail += 1
      if (nbFail > nFailLimit) {
        limitReached = true
      }
  }
  
  def reset() {
    limitReached = false
    nbFail = 0
  }
  
  
  def explore()

}

