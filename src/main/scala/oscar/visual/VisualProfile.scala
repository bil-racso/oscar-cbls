package oscar.visual

import oscar.visual._
import java.awt.geom.Line2D;

class VisualProfile(b : Boolean) extends VisualDrawing(b : Boolean) {

	private var activities : List[VisualActivity] = Nil
	private var capacity : Int = 0
	
	def setCapacity(c : Int) { capacity = c }
	def addActivity(activity : VisualActivity) { activities = activity :: activities }
	def addActivities(activities : List[VisualActivity]) { this.activities = activities }
	def resetActivities() { activities = Nil }
	
	def update(xScale : Int, yScale: Int) {
		
		var points : List[Tuple2[Int, Int]] = Nil
		
		for (act <- activities) {
			points = (act.getStart, act.getHeight) :: points
			points = (act.getEnd, -act.getHeight) :: points
		}
		
		// Sort the points
		val sortedPoints = points.sort((s, t) => s._1 < t._1)
		
		var lastPoint = (0,0)
		var nextPoint = (0,0)
		
		for (point <- sortedPoints) {
			
			if (nextPoint._1 == point._1) {
				//Edit the height of the next point to draw
				nextPoint = (point._1, nextPoint._2 + point._2)	
				
			} else {
				
				new ColoredShape[Line2D](this, new Line2D.Double(lastPoint._1*xScale, 
																 lastPoint._2*yScale, 
																 nextPoint._1*xScale, 
																 nextPoint._2*yScale))
																 
				new ColoredShape[Line2D](this, new Line2D.Double(nextPoint._1*xScale, 
																 nextPoint._2*yScale, 
																 point._1*xScale, 
																 nextPoint._2*yScale))
				
				lastPoint = (point._1, nextPoint._2)
				nextPoint = (point._1, nextPoint._2 + point._2)
				
			}
		}
		
		new ColoredShape[Line2D](this, new Line2D.Double(lastPoint._1*xScale, 
														 lastPoint._2*yScale, 
														 nextPoint._1*xScale, 
														 nextPoint._2*yScale))
	}
}