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

import java.util.Random
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.ReversibleBool

/**
 * Class representing a (reversible search) node <br>
 * A search node is used to find solution by exploration of search tree (see Search).
 * @author Pierre Schaus pschaus@gmail.com
 */
class SearchNode extends ReversibleContext {

  var silent = false

  val random: Random = new Random(0)
  val failed = new ReversibleBool(this, false)

  /**
   * @return The Random generator of this node potentially used in other algorithms
   */
  def getRandom(): Random = random

  /**
   * @return  true if this node can surely not lead to any solution
   */
  def isFailed(): Boolean = failed.value

  /**
   * Set the node in a failed state
   */
  def fail(): Unit = failed.setValue(true)

  def solFound(): Unit = {}

  override def toString(): String = super.toString

  /**
   * executed just before the actual branch action
   */
  def beforeBranch(): Unit = {}

  /**
   * executed just after the actual branch action
   */
  def afterBranch(): Unit = {}

  protected def update(): Unit = {}

  private var branchings: Branching = Branching(noAlternative)

  def search(block: => Seq[Alternative]): SearchNode = {
    branchings = Branching(block)
    this
  }

  def search(branching: Branching): SearchNode = {
    branchings = branching
    this
  }

  private var solCallBacks: List[() => Unit] = List.empty

  def onSolution(block: => Unit): SearchNode = {
    solCallBacks = (() => block) :: solCallBacks
    this
  }
  
  def clearOnsolution() {
    solCallBacks = List.empty
  }

  def beforeStartAction(): Unit = {}

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(reversibleBlock: => Unit = {}): SearchStatistics = {
    beforeStartAction()
    pushState()
    reversibleBlock
    val s = new Search(this, branchings)
    solCallBacks.foreach(b => s.onSolution(b()))
    s.onSolution(solFound())
    val stats = s.solveAll(nSols = nSols, failureLimit = failureLimit, timeLimit = timeLimit, maxDiscrepancy = maxDiscrepancy)
    stats
  }

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue): SearchStatistics = {
    startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy)()
  }
}
