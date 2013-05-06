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
package oscar.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt
import java.awt.geom.Line2D
import java.awt.Color

class VisualGanttChart(activities: Array[Activity], f : (Int) => Int, colors : (Int) => Color = i => Color.WHITE) extends VisualDrawing(false) {
	
	private val rectangles : Array[VisualRectangle] = Array.tabulate(activities.size)(a => {
		val rect = new VisualRectangle(this, 0, 0, 0, 0)
	   	rect.innerCol = colors(a)
	   	rect 
	})
	   		
	def tooltip(i: Int,s: String){ rectangles(i).toolTip = s}
	
	private val max = (0 until activities.size).map(i => f(i)).max
	
	private val text : VisualText = new VisualText(this, 50, 50, "")
	text.innerCol = Color.RED
	text.centered = true
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.outerCol = Color.RED;
	
	def maxX = makespanLine.dest._1	
	
	def update(xScale : Int, yScale: Int) {
		
		for (i <- 0 until activities.size) {
			
			rectangles(i).width = (activities(i).maxDuration)*xScale
			rectangles(i).height = yScale
			
			rectangles(i).move(activities(i).est*xScale, f(i)*yScale)
			
			rectangles(i).innerCol = colors(i)
			
		}
		
		val makespan = activities.map(_.lct).max
		
		makespanLine.orig = (makespan*xScale, 0)
		makespanLine.dest = (makespan*xScale, (max+1)*yScale)
		
		text.text = makespan.toString
		text.move(makespan*xScale, (max+2)*yScale);
		repaint()
	}
}
