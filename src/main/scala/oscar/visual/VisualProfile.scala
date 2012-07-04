package oscar.visual

import oscar.visual._
import java.awt.geom.Line2D;

class VisualProfile(b : Boolean) extends VisualDrawing(b : Boolean) {

	private var activities : List[VisualActivity] = Nil
	
	def addActivity(activity : VisualActivity) { activities = activity :: activities }
	def addActivities(activities : List[VisualActivity]) { this.activities = activities }
	def resetActivities() { activities = Nil }
	
	def update {
		
		var points : List[Tuple2[Int, Int]] = Nil
		
		for (act <- activities) {
			points = (act.getStart, act.getHeight) :: points
			points = (act.getEnd, -act.getHeight) :: points
		}
		
		// Sort the points
		val sortedPoints = points.sort((s, t) => s._1 < t._1)
		
		// First point
		var lastPoint = (0,0)
		var nextPoint = (0,0)
		
		for (point <- sortedPoints) {
			
			if (nextPoint._1 == point._1) {
				//Edit the height of the next point to draw
				nextPoint = (point._1, nextPoint._2 + point._2)	
				
			} else {
				
				new ColoredShape[Line2D](this, new Line2D.Double(lastPoint._1, lastPoint._2, nextPoint._1, nextPoint._2))
				new ColoredShape[Line2D](this, new Line2D.Double(nextPoint._1, nextPoint._2, point._1, nextPoint._2))
				
				lastPoint = (point._1, nextPoint._2)
				nextPoint = (point._1, nextPoint._2 + point._2)
				
			}
		}
		
		new ColoredShape[Line2D](this, new Line2D.Double(lastPoint._1, lastPoint._2, nextPoint._1, nextPoint._2))
	}
	
	def main(args: Array[String]) {
							  
		val frame = new VisualFrame("toto")						  
		val profile = new VisualProfile(false)
		val inf = frame.createFrame("Ressource Consumption")
		inf.add(profile)
		frame.pack
		
		val activities = List(new VisualActivity(1, 4, 1, 0),
							  new VisualActivity(2, 3, 1, 0),
							  new VisualActivity(2, 4, 2, 0),
							  new VisualActivity(5, 6, 1, 0))
		
		profile.addActivities(activities)
		profile.update
	}
}