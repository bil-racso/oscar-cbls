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

package oscar.examples.cp.scheduling

import oscar.cp.modeling._
import oscar.cp.core._

import oscar.visual._
import scala.math
import java.awt.Color
import oscar.cp.scheduling._
import oscar.cp.constraints._
import oscar.visual.shapes.VisualRectangle

/**
 * Perfect Square Problem
 *
 *  The problem is to fully cover a big 112 square with
 *  21 different smaller squares with no overlap between squares.
 *
 *  @author Pierre Schaus  pschaus@gmail.com
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

object PerfectSquare extends App {

  val s = 112
  val side = Array(50, 42, 37, 35, 33, 29, 27, 25, 24, 19, 18, 17, 16, 15, 11, 9, 8, 7, 6, 4, 2)

  val cp = CPScheduler(s)

  val nSquare = side.size
  val Square = 0 until nSquare

  val xSide = BoundedResource(cp, s, s)
  val ySide = BoundedResource(cp, s, s)

  val activitiesX = Array.tabulate(nSquare)(i => Activity(cp, side(i)))
  val activitiesY = Array.tabulate(nSquare)(i => Activity(cp, side(i)))

  cp.solve  
  cp.subjectTo {

    // Consumption
    for (i <- Square) {
      activitiesX(i) needs side(i) ofResource xSide
      activitiesY(i) needs side(i) ofResource ySide
    }

    // Overlapping
    for (i <- 0 until nSquare; j <- i + 1 until nSquare) {
      cp.post((activitiesX(i).end <== activitiesX(j).start) ||
        (activitiesX(j).end <== activitiesX(i).start) ||
        (activitiesY(i).end <== activitiesY(j).start) ||
        (activitiesY(j).end <== activitiesY(i).start))
    }

  } 
  
  cp.exploration {

    def label(w: Array[CPVarInt]) = {

      while (!cp.allBounds(w)) {

        // Minimum x position
        val pos = w.filter(!_.isBound).map(_.min).min
        val z = w.filter(w => !w.isBound && w.hasValue(pos)).head

        cp.branch(cp.post(z == pos))(cp.post(z != pos))
      }
    }

    label(activitiesX.map(_.start))
    label(activitiesY.map(_.start))
  }

  cp.run(1)

  cp.printStats()

  // Visualization
  val f = new VisualFrame("Pefect Square")
  val ff = f.createFrame("Square")
  val d = new VisualDrawing(false)
  ff.add(d)
  def scale = 5
  val bg = new VisualRectangle(d, 0, 0, s * scale, s * scale)
  bg.innerCol = Color.black
  (Square).foreach { i =>
    val r = new VisualRectangle(d, activitiesX(i).start.value * scale, activitiesY(i).start.value * scale, side(i) * scale, side(i) * scale)
    r.innerCol = VisualUtil.getRandomColor
  }
}

