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
    val nNodes: Int,
    val nFails: Int,
    val time: Long,
    val completed: Boolean,
    val timeInTrail: Long,
    val maxTrailSize: Int,
    val nSols: Int) {
  override val toString: String = s"nNodes: $nNodes\nnFails: $nFails\ntime(ms): $time\ncompleted: $completed\ntimeInTrail: $timeInTrail\nnSols: $nSols\n"
}

/**
 * DFS and Bounded Dicrepancy DFS 
 * @author Pierre Schaus pschaus@gmail.com
 */
class Search(node: SearchNode, branching: Branching) {

  type SolutionActionStat = (SearchStatistics) => Unit
  private var solutionActionsStat = List[SolutionActionStat]()
  
  /** Adds an action to execute when a solution node is found */
  def onSolution(action: => Unit) {
    solutionActionsStat = ((s: SearchStatistics) => action) :: solutionActionsStat
  }
  
  /** Adds an action to execute when a solution node is found, a statistics object on the search so far is given */
  def onSolutionWithStats(action: SearchStatistics => Unit) {
    solutionActionsStat = (action) :: solutionActionsStat
  }  

  def solFound(stat: SearchStatistics) {
    solutionActionsStat.foreach(_(stat))
  }
  
  def solveAll(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue): SearchStatistics = {
    node.resetStats()
    val t0trail = node.time
    val t0 = System.currentTimeMillis()
    def time = System.currentTimeMillis()-t0
    def timeInTrail = node.time-t0trail
    var solCounter = 0
    var nbNodes = 0
    var nBkts = 0
    var stack = scala.collection.mutable.Stack[(Int,Alternative,Boolean)]()
    
    def stat() =  new SearchStatistics(nbNodes,nFails = nBkts, time = time,completed = stack.isEmpty ,timeInTrail = timeInTrail , maxTrailSize = node.maxSize ,nSols = solCounter)

    
    val discrepancy = new ReversibleInt(node,0)
    node.pushState()
    
    
    // retrieve alternatives not exceeding max discrepancy and stack them
    def stackAlternatives(): Boolean = {
      val alts = branching.alternatives
      if (alts.isEmpty) return false
      val currDiscrepancy = discrepancy.value
      val slackDiscrepancy = maxDiscrepancy - currDiscrepancy
      var i = (maxDiscrepancy - currDiscrepancy).min(alts.size-1)
      var last = true
      while (i >= 0) {
        stack.push((i,alts(i),last))
        i -= 1
        last = false
      }
      true
    }
    
    
    
    def searchLimitReached = (time/1000 >= timeLimit) || (nBkts >= failureLimit)
    
    // add initial alternatives of the root node
    if (!node.isFailed) {
      node.pushState()
      if (!stackAlternatives()) {
         solFound(stat()) // it seems that the root node is a solution
         solCounter += 1
      }
    }
    
    var done = false
    
    while (!stack.isEmpty && !done && !searchLimitReached) {
      nbNodes += 1
      val (d,a,last) = stack.pop() // (discrepancy,alternative)
      if (!last) node.pushState()
      discrepancy.value = discrepancy.value + d
      a()
      if (!node.isFailed()) {
        // a node not failed without alternative is a solution
        if (!stackAlternatives()) { 
            solFound(stat())
            solCounter += 1
            nBkts += 1 
            if (nSols == solCounter) done = true
            else {
              node.pop()
            }
        } 
      } else {
        nBkts += 1 
        node.pop()
      }
    }
    node.popAll()
    stat()
  }

}