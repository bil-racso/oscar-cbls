package oscar.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt

import java.awt.geom.Line2D
import java.awt.Color

class VisualGanttChart(activities: Array[Activity], f : (Int) => Int, colors : (Int) => Color = i => Color.WHITE) extends VisualDrawing(false) {
	
	private val rectangles : Array[VisualRectangle] = Array.tabulate(activities.size)(a => {
		val rect = new VisualRectangle(this, 0, 0, 0, 0)
	   	rect.setInnerCol(colors(a))
	   	rect 
	})
	   												                       
	private val max = (0 until activities.size).map(i => f(i)).max
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.setOuterCol(Color.RED);
	
	def update(xScale : Int, yScale: Int) {
		
		for (i <- 0 until activities.size) {
			
			rectangles(i).setWidth((activities(i).maxDuration)*xScale)
			rectangles(i).setHeight(yScale)
			
			rectangles(i).move(activities(i).est*xScale, f(i)*yScale)
			
			rectangles(i).setInnerCol(colors(i))
		}
		
		val makespan = activities.map(_.lct).max
		
		makespanLine.setOrig(makespan*xScale, 0)
		makespanLine.setDest(makespan*xScale, (max+1)*yScale)
		
		repaint()
	}
}
