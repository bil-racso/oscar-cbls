package oscar.visual
import java.awt.Polygon;

class VisualPolygon(d : VisualDrawing) extends ColoredShape(d, new Polygon) {
	
	def reset = { shape.reset }
	
	def draw(points : Array[Tuple2[Int, Int]]) = {
		
		for (i <- 0 until points.size)
			shape.addPoint(points(i)._1, points(i)._2)
	}
	
	def update(points : Array[Tuple2[Int, Int]]) = {
		
		reset
		for (i <- 0 until points.size)
			shape.addPoint(points(i)._1, points(i)._2)
	}
}