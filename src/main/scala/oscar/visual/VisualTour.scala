package oscar.visual

import oscar.cp.core.CPVarInt
import java.awt.Color

class VisualTour(coord: Array[(Int, Int)], succ : Array[CPVarInt], dist : CPVarInt, name: String = "Tour", xScale : Int = 1, yScale : Int = 1, route : Array[CPVarInt] = null, twStart : Array[Int] = null, twEnd : Array[Int] = null) {

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
	
	val tMin = twStart.min
	val tWidth = twEnd.max - tMin

	// Draws sites
	Array.tabulate(nSites)(i => {
		
		val degStart = ((twStart(i) - tMin) / (tWidth+0.0)).toFloat
		val degEnd   = ((twEnd(i) - tMin) / (tWidth+0.0)).toFloat
		
		val c1 = new Color(degStart, 1-degStart, 0)
		val c2 = new Color(degEnd, 1-degEnd, 0)
		
		val (x, y) = coord(i)		
		new VisualCircle(drawing, x*xScale, y*yScale, 5, c2)
		new VisualCircle(drawing, x*xScale, y*yScale, 2, c1)
	})

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