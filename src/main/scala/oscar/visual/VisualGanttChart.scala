package oscar.visual

import oscar.visual._
import java.awt.geom.Line2D
import java.awt.Color

class VisualGanttChart(activities: Array[VisualActivity], cols : Array[Color], f : (Int) => Int) extends VisualDrawing(false) {
	
	private val rectangles : Array[VisualRectangle] = activities.map(a => {val rect = new VisualRectangle(this, 0, 0, 0, 0)
	   												                       rect.setInnerCol(cols(a.machine))
	   												                       rect })
	   												                       
	private val max = (0 until activities.size).map(i => f(i)).max
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.setOuterCol(Color.RED);
	
	def update(xScale : Int, yScale: Int) {
		
		for (i <- 0 until activities.size) {
			rectangles(i).setWidth((activities(i).end-activities(i).start)*xScale)
			rectangles(i).setHeight(yScale)
			rectangles(i).move(activities(i).start*xScale, f(i)*yScale)
			rectangles(i).setInnerCol(cols(activities(i).machine))
		}
		
		val makespan = activities.map(_.end).max
		
		makespanLine.setOrig(makespan*xScale, 0)
		makespanLine.setDest(makespan*xScale, max*yScale)
		
		repaint()
	}
}
