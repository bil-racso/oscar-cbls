/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.algo.search

import oscar.algo.reversible._
import scala.collection.mutable.Stack

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

  // Action to execute in case of failed or solution node
  private var solutionActions = List.empty[() => Unit]
  private var failureActions = List.empty[() => Unit]

  // Stack of alternatives to define the DFS search
  private val alternatives: Stack[(Int, Alternative, Boolean)] = Stack()

  // Number of discrepancy on the current branch of the search tree
  private val discrepancy = new ReversibleInt(node, 0)

  /** Adds an action to execute when a solution node is found */
  def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  /** Adds an action to execute when a failed node is found */
  def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  @inline private def stackAlternatives(maxDiscrepancy: Int): Boolean = {
    val alts = branching.alternatives
    if (alts.isEmpty) false
    else {
      val currDiscrepancy = discrepancy.value
      val slackDiscrepancy = maxDiscrepancy - currDiscrepancy
      var i = (maxDiscrepancy - currDiscrepancy).min(alts.size - 1)
      var last = true
      while (i >= 0) {
        alternatives.push((i, alts(i), last))
        i -= 1
        last = false
      }
      true
    }
  }

  def solveAll(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue): SearchStatistics = {

    // Initializes the search
    node.resetStats()
    alternatives.clear()
    discrepancy.value = 0

    val t0 = System.currentTimeMillis()
    var solCounter = 0
    var nbNodes = 0
    var nBkts = 0

    node.pushState()

    def searchLimitReached: Boolean = {
      ((System.currentTimeMillis() - t0) / 1000 >= timeLimit) ||
        (nBkts >= failureLimit)
    }

    if (!node.isFailed) {
      node.pushState()

      // The root is already a solution
      if (!stackAlternatives(maxDiscrepancy)) {
        solutionActions.foreach(_()) // it seems that the root node is a solution
        solCounter += 1
      }

      var done = false

      while (!alternatives.isEmpty && !done && !searchLimitReached) {
        nbNodes += 1
        val (d, a, last) = alternatives.pop() // (discrepancy,alternative)
        if (!last) node.pushState()
        discrepancy.value = discrepancy.value + d
        a()
        if (!node.isFailed()) {
          // a node not failed without alternative is a solution
          if (!stackAlternatives(maxDiscrepancy)) {
            solutionActions.foreach(_())
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
    }

    node.popAll()
    new SearchStatistics(nbNodes, nFails = nBkts, time = System.currentTimeMillis() - t0, completed = alternatives.isEmpty, timeInTrail = node.time, maxTrailSize = node.maxSize, nSols = solCounter)
  }
}
