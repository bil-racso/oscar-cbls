package oscar.visual

import oscar.visual._
import java.awt.geom.Line2D
import java.awt.Color

class VisualGanttChart(activities: Array[VisualActivity], nTasksPerJob : Int, cols : Array[Color]) extends VisualDrawing(false) {
	
	private val nJobs = activities.size/nTasksPerJob
	
	private val rectangles : Array[VisualRectangle] = activities.map(a => {val rect = new VisualRectangle(this, 0, 0, 0, 0)
	   												                       rect.setInnerCol(cols(a.machine))
	   												                       rect })
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.setOuterCol(Color.RED);
	
	def update(xScale : Int, yScale: Int) {
		
		for (i <- 0 until activities.size) {
			rectangles(i).setWidth((activities(i).end-activities(i).start)*xScale)
			rectangles(i).setHeight(activities(i).resource*yScale)
			rectangles(i).move(activities(i).start*xScale, (i/nTasksPerJob)*yScale)
		}
		
		val makespan = activities.map(_.end).max
		
		makespanLine.setOrig(makespan*xScale,0)
		makespanLine.setDest(makespan*xScale,nJobs*yScale)
		
		repaint()
	}
}
