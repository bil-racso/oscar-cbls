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


import oscar.cp._
import oscar.cp.core._
import oscar.algo.search._
import oscar.cp.scheduling._
import oscar.visual._
import scala.io.Source
import oscar.cp.scheduling.visual.VisualGanttChart




/**
 * @author Pierre Schaus
 */
object LargeScaleGreedyCumulative extends CPModel with App {

  val n = 6000
  
  val rand = new scala.util.Random(0)
  
  val durations = Array.fill(n)(rand.nextInt(200)+5)
  val height = Array.fill(n)(rand.nextInt(40)+1)
  val capa = 100
  
  

  val durationsVar = Array.tabulate(n)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(n)(t => CPIntVar(0 to 1000000000))
  val endsVar = Array.tabulate(n)(t => startsVar(t)+durationsVar(t))
  val demandsVar = Array.tabulate(n)(t => CPIntVar(height(t)))
  val resourcesVar = Array.tabulate(n)(t => CPIntVar(1))

  add(maxCumulativeResource(startsVar,durationsVar,endsVar,demandsVar,CPIntVar(capa)),Weak)
 

  val t0 = System.currentTimeMillis()
  for (i <- 0 until n) {
    if (i % 1000 == 0) println(i)
    solver.assign(startsVar(i), startsVar(i).min)
  }
  
  println(System.currentTimeMillis()-t0)
  println("max="+endsVar.map(_.value).max)

}

