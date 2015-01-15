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
 *******************************************************************************/

import oscar.cp._

/** @author Pierre Schaus */
object LargeScaleGreedyCumulative extends CPModel with App {

  val nTasks = 6000
  val rand = new scala.util.Random(0)

  val durations = Array.fill(nTasks)(rand.nextInt(200) + 5)
  val height = Array.fill(nTasks)(rand.nextInt(40) + 1)
  val horizon = 1000000000
  val capa = 100
  
  val durationsVar = Array.tabulate(nTasks)(t => CPIntervalVar(durations(t)))
  val startsVar = Array.tabulate(nTasks)(t => CPIntervalVar(0, horizon))
  val endsVar = Array.tabulate(nTasks)(t => startsVar(t) + durationsVar(t))
  val demandsVar = Array.tabulate(nTasks)(t => CPIntervalVar(height(t)))
  val capacity = CPIntVar(capa)

  add(maxCumulativeResource(startsVar, durationsVar, endsVar, demandsVar, capacity), Weak)

  val t0 = System.currentTimeMillis
  
  for (t <- 0 until nTasks) {
    if (t % 1000 == 0) println(t)
    solver.doAndPropagate {
      val variable = startsVar(t)
      variable.assign(variable.min)
    }
  }
  
  val time = System.currentTimeMillis - t0

  println(time)
  println("max=" + endsVar.map(_.value).max)
}
