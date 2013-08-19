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
  
  val js = JobShopParser.parse("data/jobshop/Lawrence/la21.txt")

  val nJobs = js.nbJobs
  val nTasksPerJob = js.nbActPerJob
  
  val nResources = nTasksPerJob
  val capacity = 2

  val nActivities = nJobs * nTasksPerJob

  val Activities = 0 until nActivities
  val Jobs = 0 until nJobs
  val Resources = 0 until nResources

  println("#Jobs       : " + nJobs)
  println("#Activities : " + nActivities)
  println("#Resources  : " + nResources)
  println("Capacity    : " + capacity)

  // Modeling	
  // -----------------------------------------------------------------------

  val horizon = js.durationMatrix.flatten.sum
  val cp = new CPScheduler(horizon)

  // Activities & Resources
  val activities = Array.tabulate(nJobs,nTasksPerJob){case(i,j) => Activity(cp, js.durationMatrix(i)(j))}
  val resources = Array.tabulate(nResources)(r => MaxResource(cp, 2))

  // Resource allocation
  for (i <- 0 until nJobs; j <- 0 until nTasksPerJob) {
    activities(i)(j) needs 1 ofResource resources(js.jobMatrix(i)(j))
  }

  // The makespan to minimize
  val makespan = maximum(0 until nJobs)(i => activities(i)(nTasksPerJob-1).end)

  // Visualization  
  // -----------------------------------------------------------------------

  val frame = new VisualFrame("Cumulative JobShop Problem", nResources + 1, 1)
  val colors = VisualUtil.getRandomColors(nResources, true)

  val jobsFlat = js.jobMatrix.flatten

  val gantt = new VisualGanttChart(activities.flatten, i => i/nTasksPerJob, colors = i => colors(jobsFlat(i)))
  val profiles = Array.tabulate(nResources)(i => new VisualProfile(resources(i), makespan, color = colors(i)))


  frame.createFrame("Gantt chart").add(gantt)
  for (p <- profiles) frame.createFrame(p.resource.toString).add(p)
  frame.pack

  // Constraints & Search
  // -----------------------------------------------------------------------
  
  cp.minimize(makespan) subjectTo {

    for (i <- 0 until nJobs; j <- 0 until nTasksPerJob-1)
      activities(i)(j) endsBeforeStartOf activities(i)(j + 1)
  } exploration {

    cp.setTimes(activities.flatten)

    // record partial order schedule of current best sol
    resources.foreach(_.recordPartialOrderSchedule())

    for (p <- profiles) p.update(1, 20)
    gantt.update(1, 10)
  } run (1)

  var limit = 2000
  for (i <- 0 until 2000) {
    val temp = limit

    // Adaptative LNS
    limit = if (!cp.explorationCompleted) limit * 110 / 100 else max(10, (limit * 90) / 100)

    // 10% of  activities are relaxed
    val relaxed = activities.flatten.filter(i => nextFloat < 0.1).toSet

    cp.runSubjectTo(Int.MaxValue, limit) {
      for (r <- resources) {
        cp.post(r.partialOrderSchedule(relaxed))
      }
    }
  }

  cp.printStats()
}

