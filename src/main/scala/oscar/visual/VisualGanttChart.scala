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
	   												                       
	private val max = (0 until activities.size).map(i => f(i)).max
	
	private val text : VisualText = new VisualText(this, 50, 50, "")
	text.innerCol = Color.RED
	text.centered = true
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.outerCol = Color.RED;
	
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
