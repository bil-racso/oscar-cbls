package oscar.visual

import oscar.cp.core.CPVarInt
import java.awt.Color

class VisualTour(coord: Array[(Int, Int)], succ : Array[CPVarInt], dist : CPVarInt, name: String = "Tour", xScale : Int = 1, yScale : Int = 1, route : Array[CPVarInt] = null) {

	// Initialize
	// ----------------------------------------------------
	
	var nRoute = 0
	if (route != null) nRoute = route.size
	
	var col = VisualUtil.getRandomColorArray(nRoute)

	val nSites = coord.size
	val Sites  = 0 until nSites
	
	val frame = new VisualFrame(name)
	
	// Creates the plot and place it into the frame
	val plot = new Plot2D("", "Solution number", "Distance")
	frame.createFrame("Tour length").add(plot)

	// Creates the tour representation and place it into the frame
	val drawing = new VisualDrawing(false)
	
	// Routes
	val lines = Array.tabulate(nSites)(i => new VisualLine(drawing, coord(i)._1*xScale, coord(i)._2*yScale, coord(i)._1*xScale, coord(i)._2*yScale))

	frame.createFrame("TSP Tour").add(drawing)
	frame.pack()

	var nbSol = 0

	// Draws sites
	coord.foreach(c => new VisualCircle(drawing, c._1*xScale, c._2*yScale, 2, Color.blue))

	def update {
		
		nbSol += 1
		
		for (i <- Sites) {
			
			if (nRoute > 0) {
				lines(i).setInnerCol(col(route(i).value))
				lines(i).setOuterCol(col(route(i).value))
			}
			
			val x = coord(succ(i).value)._1 * xScale
			val y = coord(succ(i).value)._2 * yScale
			
			lines(i).setDest(x, y)
		}
		
		drawing.repaint()
		plot.addPoint(nbSol, dist.value)
	}
}