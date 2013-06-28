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

package oscar.cp.scheduling

import scala.collection.mutable.Map
import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue
import oscar.cp.core.Constraint

class CumulativeResource(scheduler: CPScheduler, maxCapa: Int = Int.MaxValue, minCapa: Int = Int.MinValue, n: String = null) extends Resource(scheduler, n = n) {

  protected val activitiesSet: Map[Activity, CumulativeActivity] = Map()

  def heightOf(act: Activity): CPVarInt = activitiesSet(act).height

  def capacity = maxCapa

  def cumulativeActivities = activitiesSet.values.toArray


  val rand = new scala.util.Random()

  override def setup() {
    val act = activitiesSet.values.toArray
    if (act.size > 0) {
      if (minCapa != Int.MinValue && maxCapa != Int.MaxValue) {
        scheduler.add(cumulative(act, id, max = maxCapa, min = minCapa))
      } else if (minCapa == Int.MinValue && maxCapa != Int.MaxValue) {
        scheduler.add(cumulative(act, id, max = maxCapa))
      } else if (minCapa != Int.MinValue && maxCapa == Int.MaxValue) {
        scheduler.add(cumulative(act, id, min = minCapa))
      } else throw new InvalidParameterException("cumulative constraint bounded between -Infinity and Infinity")
    }
  }

  // Adding an activity
  def addActivity(act: Activity, cum: CumulativeActivity) {
    if (activitiesSet.contains(act))
      throw new InvalidParameterException("The activity is already scheduled on this resource.")
    else
      activitiesSet += (act -> cum)
  }
  
  var posPrec: Set[(Activity, Activity)] = Set()
  
  def recordPartialOrderSchedule() {
    posPrec = precedences()
  }
  
  def partialOrderSchedule(relaxed: Set[Activity]) = {
    val filteredPrecedences = posPrec.filter{case(a,b) => !relaxed.contains(a) && !relaxed.contains(b)}
    val constraints = filteredPrecedences.map{case(a,b) => a.end <= b.start}
    constraints.toArray[Constraint]
  }

  def precedences(): Set[(Activity, Activity)] = {
    
    val (activitiesOrig, activities) = {
    	val (a, b) = activitiesSet.unzip
    	(a.toArray, b.toArray)
    }
    
    //def heightOf(act: Activity): CPVarInt = activitiesSet(act).height
    def heightOf(i: Int): CPVarInt = activities(i).height

    // Sort the tasks by start, inc and random as tie breaker
    val sortedTasks = (0 until activities.length).sortBy(t => (activities(t).est, -heightOf(t).min, rand.nextInt))
    


    // tasks = id,demand
    def divide(tasks: Array[(Int, Int)], limit: Int): Set[(Int, Int)] = {
      if (!stopCriterion(tasks, limit)) {

        val limit1 = scala.math.ceil(limit / 2).toInt
        val limit2 = limit - limit1

        val gapStruct1 = new GapStructure(limit1)
        val gapStruct2 = new GapStructure(limit2)

        val tasks1: Queue[(Int, Int)] = Queue()
        val tasks2: Queue[(Int, Int)] = Queue()

        for ((t, c) <- tasks) {

          val act = activities(t)

          val gap1 = gapStruct1.gap(act.est)
          val gap2 = gapStruct2.gap(act.est)

          val (minQueue, minStruct, minGap, maxQueue, maxStruct, maxGap) =
            if (gap1 < gap2) {
              (tasks1, gapStruct1, gap1, tasks2, gapStruct2, gap2)
            } else {
              (tasks2, gapStruct2, gap2, tasks1, gapStruct1, gap1)
            }

          if (maxGap >= c) {

            maxStruct.add(act.lct, c)
            maxQueue.enqueue((t, c))

          } else {

            val r = c - maxGap

            maxStruct.add(act.lct, maxGap)
            maxQueue.enqueue((t, maxGap))
            minStruct.add(act.lct, r)
            minQueue.enqueue((t, r))
          }
        }
        return divide(tasks1.toArray, limit1) union divide(tasks2.toArray, limit2)

      } else {
        return if (tasks.size <= 1) Set() else tasks.sliding(2).map(a => (a(0)._1, a(1)._1)).toSet
      }
    }

    divide(sortedTasks.map(t => (t, heightOf(t).min)).toArray, maxCapa).map{case(a,b) => (activitiesOrig(a),activitiesOrig(b))}

  }

  def stopCriterion(tasks: Array[(Int, Int)], capacity: Int): Boolean = {

    if (tasks.size <= 2)
      return true

    val s = tasks.sortBy(_._2)

    if (s(0)._2 + s(1)._2 > capacity)
      return true

    return false
  }

  // This class allows to efficiently compute the gap at a specific time
  class GapStructure(limit: Int) {

    private val ordering = new Ordering[Tuple2[Int, Int]] {
      def compare(a: Tuple2[Int, Int], b: Tuple2[Int, Int]) = if (b._2 > a._2) { 1 } else if (b._2 == a._2) { 0 } else { -1 }
    }
    private val eventPointSeries = new PriorityQueue[Tuple2[Int, Int]]()(ordering)
    private var gap = limit

    def gap(date: Int): Int = {
      while (!eventPointSeries.isEmpty && eventPointSeries.head._2 <= date)
        gap += eventPointSeries.dequeue()._1

      return gap
    }

    def add(end: Int, inc: Int) {
      gap -= inc
      eventPointSeries.enqueue((inc, end))
    }
  }

}

object MaxResource {

  def apply(scheduler: CPScheduler, capa: Int, name: String = null) = new CumulativeResource(scheduler, maxCapa = capa, n = name)
}

object MinResource {

  def apply(scheduler: CPScheduler, capa: Int, name: String = null) = new CumulativeResource(scheduler, minCapa = capa, n = name)
}

object ContainerResource {

  def apply(scheduler: CPScheduler, maxCapa: Int, name: String = null) = new CumulativeResource(scheduler, maxCapa, 0, name)
}

object BoundedResource {

  def apply(scheduler: CPScheduler, maxCapa: Int, minCapa: Int, name: String = null) = new CumulativeResource(scheduler, maxCapa, minCapa, name)
}
