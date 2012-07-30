package oscar.visual

import java.awt.geom.Line2D
import java.awt.Color

import oscar.algo.CumulativeProfile

class VisualProfile(allActivities: Array[VisualActivity], resource : Int, private var c : Int, col : Color) extends VisualDrawing(false, true) {
	
	private var activities : Set[VisualActivity] = Set()
	
	// The profile is represented by a polygon
	private val polygon : VisualPolygon = new VisualPolygon(this)
	polygon.setInnerCol(col)
	
	// The limit of capacity
	private val line : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	line.setOuterCol(Color.RED);
	
	def capacity = c
	def capacity_= (x : Int) { c = x }
		
	def addActivity(activity : VisualActivity) = { activities =  activities + activity }
	
	def removeActivity(activity : VisualActivity) = { activities = activities - activity }
	
	def updateActivities() { activities = allActivities.filter(a => a.machine == resource).toSet }
	
	def update(xScale : Int, yScale: Int) {
		
		updateActivities
			
		val points = CumulativeProfile.getCumulativeProfile(activities)
		
		polygon.update(points.map(p => (p._1*xScale, (p._2 + 5)*yScale)))
		
		val makespan = allActivities.map(_.end).max
		
		line.setOrig(0, (capacity+5)*yScale)
		line.setDest(xScale*makespan, (capacity+5)*yScale)
		
		repaint()
	}
}
