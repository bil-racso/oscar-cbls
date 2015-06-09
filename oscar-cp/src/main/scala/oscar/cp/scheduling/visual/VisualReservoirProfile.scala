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
package oscar.cp.scheduling.visual

import java.awt.Color

import oscar.cp.core.CPSolver
import oscar.cp.core.variables.CPIntVar
import oscar.visual.{VisualFrame, VisualDrawing}
import oscar.visual.shapes.{VisualLine, VisualPolygon}
import oscar.cp._

/**
 * Created on 08/06/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */

/**
 * @constructor Create a new graphical profile for reservoir resource with specified parameters.
 *
 * @param startVars Variables for task starting times
 * @param durationVars Variables for task durations
 * @param endVars Variables for task ending times
 * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
 * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
 * @param minCapacity The minimal capacity of the reservoir
 * @param maxCapacity The maximal capacity of the reservoir
 * @param initialAmount The initial amount of resource in the reservoir
 * @param color The color under the profile curve
 */
class VisualReservoirProfile(startVars: Array[CPIntVar], durationVars: Array[CPIntVar],
                             endVars: Array[CPIntVar], productionVars: Array[CPIntVar],
                             consumptionVars: Array[CPIntVar], minCapacity: Int,
                             maxCapacity: Int, initialAmount: Int = 0,
                             color: Color = Color.WHITE) extends VisualDrawing(true, false) {

  private[this] val nTasks = startVars.length

  def producer(index: Int) = productionVars(index).max > 0
  def consumer(index: Int) = consumptionVars(index).max > 0

  // The profile is represented by a polygon
  private val polygon: VisualPolygon = VisualPolygon(this)
  polygon.innerCol = color
  polygon.autoRepaint = false

  // The capacity lower limit
  private val minCapaLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  minCapaLine.outerCol = Color.RED
  minCapaLine.borderWidth_=(2F)
  minCapaLine.toolTip = "Min Capacity"

  // The capacity upper limit
  private val maxCapaLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  maxCapaLine.outerCol = Color.RED
  maxCapaLine.borderWidth_=(2F)
  maxCapaLine.toolTip = "Max Capacity"

  // The zero line
  private val zeroLine: VisualLine = VisualLine(this, 0, 0, 0, 0)
  zeroLine.outerCol = Color.BLUE


  def update(xScale: Int, yScale: Int) {
    val events = (0 until nTasks).map(index => if (producer(index)) (index, endVars(index).max) else (index, startVars(index).min)).sortBy(pair => pair._2)
    var i = 0
    var points = List[(Int, Int)]()
    var height = initialAmount

    val start = startVars.map(variable => variable.value).min
    val end = endVars.map(variable => variable.value).max

    points ::= (start, 0)
    points ::= (start, initialAmount)

    while (i < nTasks) {
      points ::= (events(i)._2, height)
      if (producer(events(i)._1)) {
        height += productionVars(events(i)._1).max
      }
      else if (consumer(events(i)._1)) {
        height -= consumptionVars(events(i)._1).max
      }
      points ::= (events(i)._2, height)
      i += 1
    }

    points ::= (end, height)
    points ::= (end, 0)

    val min = points.map(_._1).min
    val max = points.map(_._1).max

    polygon.update(points.reverse.map(p => (p._1 * xScale, (p._2 + min) * yScale)))

    minCapaLine.orig = (0, minCapacity * yScale)
    minCapaLine.dest = (xScale * max, minCapacity * yScale)

    maxCapaLine.orig = (0, maxCapacity * yScale)
    maxCapaLine.dest = (xScale * max, maxCapacity * yScale)

    zeroLine.orig = (0, min * yScale)
    zeroLine.dest = (xScale * max, min * yScale)

    repaint()
  }
}

object VisualReservoirProfile extends App {
  implicit val cp = CPSolver()

  // producer
  val s1 = CPIntVar(0)
  val d1 = CPIntVar(10)
  val e1 = s1 + d1
  val p1 = CPIntVar(2)
  val c1 = CPIntVar(0)

  // consumer
  val s2 = CPIntVar(15)
  val d2 = CPIntVar(20)
  val e2 = s2 + d2
  val p2 = CPIntVar(0)
  val c2 = CPIntVar(3)

  val initialAmount = 5
  val minCapacity = 3
  val maxCapacity = 8

  val frame = new VisualFrame("Test Reservoir Profile", 1, 1)
  val f1 = frame.createFrame("Reservoir Profile")
  val vp = VisualReservoirProfile(Array(s1, s2), Array(d1, d2), Array(e1, e2), Array(p1, p2), Array(c1, c2), minCapacity, maxCapacity, initialAmount, Color.CYAN)
  f1.add(vp)
  f1.pack()
  frame.pack()
  vp.update(20, 20)

  /**
   * @constructor Create a new graphical profile for reservoir resource with specified parameters.
   *
   * @param startVars Variables for task starting times
   * @param durationVars Variables for task durations
   * @param endVars Variables for task ending times
   * @param productionVars Variables for task productions; represents the amounts of the resource produced by tasks
   * @param consumptionVars Variables for task consumptions; represents the amounts of the resource consumed by tasks
   * @param minCapacity The minimal capacity of the reservoir
   * @param maxCapacity The maximal capacity of the reservoir
   * @param initialAmount The initial amount of resource in the reservoir
   * @param color The color under the profile curve
   */
  def apply(startVars: Array[CPIntVar], durationVars: Array[CPIntVar],
                             endVars: Array[CPIntVar], productionVars: Array[CPIntVar],
                             consumptionVars: Array[CPIntVar], minCapacity: Int,
                             maxCapacity: Int, initialAmount: Int = 0,
                             color: Color = Color.WHITE): VisualReservoirProfile = {
    new VisualReservoirProfile(startVars, durationVars, endVars, productionVars,
                                consumptionVars, minCapacity, maxCapacity, initialAmount, color)
  }

}
