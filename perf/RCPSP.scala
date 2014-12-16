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

import oscar.cp.modeling._
import oscar.cp.modeling.Constraints
import oscar.cp.core._

/**
 *
 *  @authors: Pierre Schaus pschaus@gmail.com
 */
object RCPSP {

  def main(args: Array[String]) {

    // (duration, consumption)
    val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2), (20, 2), (80, 1))
    val durationsData = instance.map(_._1)
    val demandsData = instance.map(_._2)
    val capa = 4
    val horizon = instance.map(_._1).sum
    val Times = 0 to horizon
    val nTasks = instance.size

    implicit val cp = CPSolver()
    cp.silent = true

    val durations = Array.tabulate(nTasks)(t => CPIntervalVar(durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPIntervalVar(0, horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPIntervalVarViewOffset(starts(t), durationsData(t)))
    val demands = Array.tabulate(nTasks)(t => CPIntervalVar(demandsData))

    val makespan = maximum(ends)
    add(maxCumulativeResource(starts, durations, ends,demands, CPIntervalVar(capa)))
    
    minimize(makespan) search {
      setTimes(starts, durations,ends)
    }
    println(cp.start())

  }
}
