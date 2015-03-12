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


package oscar.cp.searches


import oscar.cp._
import oscar.algo.reversible._
import oscar.algo.search.Branching
import oscar.cp.core.CPOutcome.Failure
import oscar.util.IncrementalStatistics
import scala.util.Random;
/**
 *
 * @author Pierre Schaus
 */
class BinaryABS(variables: Array[CPIntVar], valHeuristic: Int => Int,rand: Random,nProbes: Int = 1000, decay: Double = 0.999) extends Branching {

  private[this] val s = variables(0).store
  private[this] val n = variables.size
  private[this] val activity = Array.fill(n)(0.0);
  private[this] val domSize = Array.tabulate(n)(i => variables(i).size);
  private[this] val stats = Array.fill(n)(new IncrementalStatistics()) // one stat for each variables
  
  final override def alternatives: Seq[Alternative] = {

    for (i <- 0 until n) {
      if (variables(i).size < domSize(i)) {
        activity(i) += 1;
      } else if (!variables(i).isBound) {
        activity(i) *= decay;
      }
      domSize(i) = variables(i).size
    }
    
    var i = 0
    var maxActivity = Double.MinValue
    var varIdx = -1
    
    while (i < n) {
      if (!variables(i).isBound && activity(i) > maxActivity) {
        varIdx = i
        maxActivity = activity(i)
      }
      i += 1
    }
    if (varIdx == -1) return noAlternative
    else {
      val v = valHeuristic(varIdx)
      branch(s.assign(variables(varIdx),v))(s.remove(variables(varIdx),v))
    }
  }

  override def reset() {
    for (i <- 0 until n) {
      stats(i).reset()
      domSize(i) = variables(i).size
    }
    for (i <- 0 until nProbes) {
      probe()
      updateStatisticsAndReset()
    }
    for (i <- 0 until n) {
      activity(i) = stats(i).average
    }
  }
  
  private def updateStatisticsAndReset() {
    for (i <- 0 until n) {
      stats(i).addPoint(activity(i))
      activity(i) =  0
      domSize(i) = variables(i).size
    }
  }  
  
  val indexes = Array.tabulate(n)(i => i).toSeq
  private def probe() {
    s.pushState()
    val shuffled = rand.shuffle(indexes)
    var i = 0
    while (i < n && !s.isFailed) {
      val x = variables(shuffled(i))
      if (!x.isBound) {
        if (s.assign(x,valHeuristic(shuffled(i))) != Failure)
        if (!s.isFailed()) {
          updateActivities()
        }
      }
      i += 1
    }
    s.pop()
  }

  private def updateActivities() {
    for (i <- 0 until n) {
      if (variables(i).size < domSize(i)) {
        activity(i) += 1;
      }
      domSize(i) = variables(i).size
    }
  }  
  

  
}
