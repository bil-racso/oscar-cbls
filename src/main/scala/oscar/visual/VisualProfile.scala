/*******************************************************************************
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
 ******************************************************************************/

package oscar.visual

import java.awt.geom.Line2D
import java.awt.Color

import oscar.algo.CumulativeProfile
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt

class VisualProfile(res : CumulativeResource, makespan : CPVarInt, color : Color = Color.WHITE) extends VisualDrawing(true) {
	
	// The profile is represented by a polygon
	private val polygon : VisualPolygon = new VisualPolygon(this)
	polygon.innerCol = color
	
	// The capacity limit
	private val capaLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	capaLine.outerCol = Color.RED;
	
	// The zero line
	private val zeroLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	zeroLine.outerCol = Color.BLUE;
	
	def resource = res
	
	def update(xScale : Int, yScale: Int) {
			
		val activities = resource.cumulativeActivities
		val rawPoints  = CumulativeProfile.getCumulativeProfile(activities)
		
		// The end of a ProdConsActivity is not relevant
		val points = rawPoints.map(p => if (p._1 > makespan.min) (makespan.min, p._2) else p)
		
		val min = -points.map(_._2).min
		
		polygon.update(points.map(p => (p._1*xScale, (p._2 + min)*yScale)))

		capaLine.orig = (0, (resource.capacity + min)*yScale)
		capaLine.dest = (xScale*makespan.getMax, (resource.capacity + min)*yScale)
		
		zeroLine.orig = (0, (min)*yScale)
		zeroLine.dest = (xScale*makespan.getMax, (min)*yScale)
		
		repaint()
	}
}
