/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cp.scheduling

import oscar.cp.constraints.NewMaxCumulative
import java.awt.Dimension
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.scheduling._
import oscar.reversible.ReversibleSetIndexedArray
import oscar.reversible.ReversibleInt
import oscar.search._
import oscar.visual._
import scala.util.Random.nextFloat

import scala.io.Source
import scala.collection.mutable.Set
import scala.math.max

object CumulativeJobShopLNS extends App {

  // Parsing		
  // -----------------------------------------------------------------------

  var lines = Source.fromFile("data/jobshop/Lawrence/la12.txt").getLines.toList 
  lines = lines.drop(3)
  
  val size = lines.head.trim().split(" ")
  val nJobs = size(0).toInt
  val nResources = size(1).toInt 
  val nActivities = nResources

  val Activities = 0 until nActivities
  val Jobs = 0 until nJobs
  val Resources = 0 until nResources

  lines = lines.drop(1)
  
  val assign = Array.fill(nJobs, nActivities)(0)
  val durations = Array.fill(nJobs, nActivities)(0)

  for (j <- Jobs) {
    val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
    for (a <- Activities) {
      assign(j)(a) = l(a*2)
      durations(j)(a) = l(a*2+1)
    }
    lines = lines.drop(1)
  }

  // Modeling	
  // -----------------------------------------------------------------------

  val horizon = durations.flatten.sum
  val cp = new CPScheduler(horizon)

  // Activities & Resources
  val activities = Array.tabulate(nJobs, nActivities)((j, a) => Activity(cp, durations(j)(a)))
  val resources = Array.tabulate(nResources)(r => MaxResource(cp, 2))

  // Resource allocation
  for (j <- Jobs; a <- Activities) {
    activities(j)(a) needs 1 ofResource resources(assign(j)(a))
  }

  // The makespan to minimize 
  val flattenAct = activities.flatten
  val makespan = maximum(0 until flattenAct.size)(a => flattenAct(a).end)

  // Visualization  
  // -----------------------------------------------------------------------

  val frame = new VisualFrame("Cumulative JobShop Problem")
  frame.setPreferredSize(new Dimension(1024, 768))
  val colors = VisualUtil.getRandomColorArray(nResources)

  val flattenAssign = assign.flatten
  val gantt = new VisualGanttChart(flattenAct, i => i/nActivities, colors = i => colors(flattenAssign(i)))
  
  frame.add(gantt)
  frame.pack

  // Constraints & Search
  // -----------------------------------------------------------------------

  val bestSol: Array[FixedActivity] = Array.tabulate(flattenAct.size)(i => new FixedActivity(i, 0, 0, 0, 0))
  var precedences: Array[(Int, Int)] = null

  cp.lns(2000, 2000) {

    val temp = cp.failLimit

    val selected: Array[Boolean] = Array.fill(bestSol.size)(false)

    // Selected are relaxed (20%)
    for (i <- 0 until bestSol.size)
      if (nextFloat < 0.2)
        selected(i) = true

    val filteredPrecedences = precedences.filter(p => !selected(p._1) && !selected(p._2))
    val constraints = filteredPrecedences.map(p => flattenAct(p._1).end <= flattenAct(p._2).start)

    cp.post(constraints.asInstanceOf[Array[Constraint]])
  }

  cp.minimize(makespan) subjectTo {
    
    for (j <- Jobs; a <- 1 until nActivities)
      activities(j)(a-1) precedes activities(j)(a)

  } exploration {
  
    cp.setTimes(flattenAct)

    // Best so far solution
    for (t <- 0 until flattenAct.size) {

      bestSol(t).start = flattenAct(t).est
      bestSol(t).end = flattenAct(t).lct
      bestSol(t).inc = 1
      bestSol(t).machine = flattenAssign(t)
    }

    precedences = PartialOrderSchedule.getPrecedences(bestSol, Array.fill(nResources)(2))

    gantt.update(1, 20)
  }

  cp.printStats()
}

