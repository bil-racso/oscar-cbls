/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
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

package oscar.algo.search

import oscar.algo.reversible._

/**
 * DFS and Bounded Dicrepancy DFS 
 * @author: Pierre Schaus pschaus@gmail.com
 */
class Search(node: SearchNode, branching: Branching) {
  type SolutionAction = () => Unit
  private var solutionActions = List[SolutionAction]()

  def onSolution(action: => Unit) {
    solutionActions = (() => action) :: solutionActions
  }

  def solFound() {
    solutionActions.foreach(_())
  }

  def solveAll(nbSol: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue): List[(String,Int)] = {
   
    var stack = scala.collection.mutable.Stack[(Int,Alternative)]()
    val discrepancy = new ReversibleInt(node,0)
    node.pushState()
    
    
    // retrieve alternatives not exceeding max discrepancy and stack them
    def stackAlternatives(): Boolean = {
      val alts = branching.alternatives
      if (alts.isEmpty) return false
      val currDiscrepancy = discrepancy.value
      val slackDiscrepancy = maxDiscrepancy - currDiscrepancy
      var i = (maxDiscrepancy - currDiscrepancy).min(alts.size-1)
      while (i >= 0) {
        stack.push((i,alts(i)))
        i -= 1
      }
      true
    }
    
    var solCounter = 0
    var nbNodes = 0
    var nbBkts = 0
    
    // add initial alternatives of the root node
    stackAlternatives()
    node.pushState()
    var done = false
    while (!stack.isEmpty && !done) {
      nbNodes += 1
      val (d,a) = stack.pop() // (discrepancy,alternative)
      discrepancy.value = discrepancy.value + d
      a()
      if (!node.isFailed()) {
        if (!stackAlternatives()) {
            solFound()
            solCounter += 1
            nbBkts += 1 
            if (nbSol == solCounter) done = true
            else node.pop()
        } else {
            node.pushState()
        }

      } else {
        nbBkts += 1 
        node.pop()
      }
    }
    List(("#sol",solCounter),("#bkts",nbBkts),("#nodes",nbNodes))
  }

}