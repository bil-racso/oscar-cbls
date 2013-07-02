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

import java.awt.Color

import oscar.cp.scheduling._
import oscar.visual._

class VisualGanttChart(activities: Array[_ <: Activity], f : (Int) => Int, colors : (Int) => Color = i => Color.WHITE) extends VisualDrawing(false) {
	
	private val rectangles = Array.fill(activities.size)(new VisualRectangle(this))	
	rectangles.indices.foreach(i => rectangles(i).innerCol = colors(i))
	
    private val max = (0 until activities.size).map(i => f(i)).max
    
	private val timeWindow = Array.fill(activities.size)(new VisualLine(this))
	private val swimLanes = Array.fill(max)(new VisualLine(this))
	
	private val text : VisualText = new VisualText(this, 50, 50, "")
	text.innerCol = Color.RED
	text.centered = true
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.outerCol = Color.RED;
	
	private var _showMakespan = true
	def showMakespan = _showMakespan
	
	def showMakespan_= (x: Boolean): Unit = {
	  _showMakespan = x
	  makespanLine.visible = _showMakespan
	  text.visible = _showMakespan
	}
	
	def update(xScale : Int = 100, yScale: Int = 50)  {
		
		val makespan = activities.map(_.lct).max
		val maxRow = activities.indices.map(f(_)).max
	  
		for (i <- 0 until activities.size) {
			val row = f(i) * yScale + (0.1 * yScale)
			
			rectangles(i).width = (activities(i).maxDuration) * xScale
			rectangles(i).height = yScale * 0.8
			
			rectangles(i).move(activities(i).est * xScale, row)
			
			rectangles(i).innerCol = colors(i)
			
			timeWindow(i).orig = (activities(i).est * xScale, (row + 0.5 * yScale))
			timeWindow(i).dest = (activities(i).lct * xScale, (row + 0.5 * yScale))
		}
		
		for (i <- swimLanes.indices) {
		    val row = i * yScale
			swimLanes(i).orig = (0, row)
			swimLanes(i).dest = (makespan * yScale, row)
		}

		
		if(showMakespan){
			makespanLine.orig = (makespan*xScale, 0)
			makespanLine.dest = (makespan*xScale, (max+1)*yScale)
			
			makespanLine.visible = showMakespan
			
			text.text = makespan.toString
			text.move(makespan * xScale, maxRow * yScale);
		}
		repaint()
		
	}
}
