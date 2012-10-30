package oscar.visual

import java.awt.geom.Line2D
import java.awt.Color

import oscar.algo.CumulativeProfile
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt

class VisualProfile(res : CumulativeResource, makespan : CPVarInt, color : Color = Color.WHITE) extends VisualDrawing(false, true) {
	
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
			
		val activities = resource.activities
		val rawPoints  = CumulativeProfile.getCumulativeProfile(activities)
		
		// The end of a ProdConsActivity is not relevant
		val points = rawPoints.map(p => if (p._1 > makespan.min) (makespan.min, p._2) else p)
		
		val min = -points.map(_._2).min
		
		polygon.update(points.map(p => (p._1*xScale, (p._2 + min)*yScale)))

		capaLine.setOrig(0, (resource.capacity + min)*yScale)
		capaLine.setDest(xScale*makespan.getMax, (resource.capacity + min)*yScale)
		
		zeroLine.setOrig(0, (min)*yScale)
		zeroLine.setDest(xScale*makespan.getMax, (min)*yScale)
		
		repaint()
	}
}
