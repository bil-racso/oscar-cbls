package oscar.visual

import oscar.visual._
import java.awt.geom.Line2D
import java.awt.Color

import oscar.algo.CumulativeProfile

class VisualProfile(private var activities: Set[VisualActivity], col : Color) extends VisualDrawing(false, true) {
	
	private var c : Int = _
	
	private val polygon : VisualPolygon = new VisualPolygon(this)
	private val line    : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	
	def capacity = c
	def capacity_= (x : Int) { c = x }
	
	def addActivity(activity : VisualActivity) = { activities =  activities + activity }
	def removeActivity(activity : VisualActivity) = { activities = activities - activity }
	
	def draw(xScale : Int, yScale: Int) {
		
		val points  = CumulativeProfile.getCumulativeProfile(activities)
		polygon.draw(points.map(p => (p._1*xScale, p._2*yScale)))
		polygon.setInnerCol(col)
		
		line.setOrig(xScale*points(0)._1, yScale*capacity)
		line.setDest(xScale*points(points.size-1)._1, yScale*capacity)
		line.setOuterCol(Color.RED);
		
		repaint()
	}
	
	def update(xScale : Int, yScale: Int) {
		
		val points  = CumulativeProfile.getCumulativeProfile(activities)
		
		polygon.update(points.map(p => (p._1*xScale, p._2*yScale)))
		
		line.setOrig(xScale*points(0)._1, yScale*capacity)
		line.setDest(xScale*points(points.size-1)._1, yScale*capacity)
		
		repaint()
	}
}
