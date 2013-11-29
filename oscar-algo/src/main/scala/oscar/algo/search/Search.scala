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

class SearchStatistics(
    val nbNodes: Int,
    val nbFails: Int,
    val time: Long,
    val completed: Boolean,
    val timeInTrail: Long,
    val maxTrailSize: Int,
    val nbSols: Int) {
  override def toString: String = {
    "nbNode: "+nbNodes+" \n" + "nbFails: "+nbFails+" \n" + "time(ms): "+time+" \n" + "completed: "+completed+" \n" + "timeInTrail: "+timeInTrail+" \n" + "nbSols: "+nbSols+" \n"
    
  }
}



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

  def solveAll(nbSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue): SearchStatistics = {
    val t0trail = node.trail.getTimeInRestore()
    val t0 = System.currentTimeMillis()
    node.trail.resetStats()
    def time = System.currentTimeMillis()-t0
    def timeInTrail = node.trail.getTimeInRestore()-t0trail
    
    
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
    
    def searchLimitReached = time/1000 >= timeLimit || nbBkts >= failureLimit
    
    // add initial alternatives of the root node
    stackAlternatives()
    node.pushState()
    var done = false
    while (!stack.isEmpty && !done && !searchLimitReached) {
      nbNodes += 1
      val (d,a) = stack.pop() // (discrepancy,alternative)
      discrepancy.value = discrepancy.value + d
      a()
      if (!node.isFailed()) {
        if (!stackAlternatives()) {
            solFound()
            solCounter += 1
            nbBkts += 1 
            if (nbSols == solCounter) done = true
            else node.pop()
        } else {
            node.pushState()
        }

      } else {
        nbBkts += 1 
        node.pop()
      }
    }
    node.popAll()
    new SearchStatistics(nbNodes,nbFails = nbBkts, time = time,completed = stack.isEmpty ,timeInTrail = timeInTrail , maxTrailSize = node.trail.getMaxSize() ,nbSols = solCounter)
  }

}