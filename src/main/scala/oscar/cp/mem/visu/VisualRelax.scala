package oscar.cp.mem.visu

import oscar.visual._
import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt.Graphics

class VisualRelax(val coord : Array[(Int, Int)], distances : Array[Array[Double]]) extends VisualFrame("Relaxation Viewer") {

	// Some constants
	val nSites = coord.size
	val Sites  = 0 until nSites
	
	val zero = 20
	val selectCol    = Color.RED
	val notSelectCol = Color.WHITE
	val normalCol    = Color.BLACK
	
	val maxX = coord.map(_._1).max
	val maxY = coord.map(_._2).max

	setPreferredSize(new Dimension(400, 400))
	val drawing = new VisualDrawing(false)
	
	val xScale = 0
	val yScale = 0
	
	var prev : Array[Int] = Array.tabulate(nSites)(i => i)
	var next : Array[Int] = Array.tabulate(nSites)(i => i)
	var dist = 0.0
		
	// Routes
	val lines = Array.tabulate(nSites)(i => {
		val (x, y) = coord(i)
		new VisualLine(drawing, x*xScale + zero, y*yScale + zero, x*xScale + zero, y*yScale + zero)
	})

	// Sites
	val circles = Array.tabulate(nSites)(i => {
		val (x, y) = coord(i)
		new VisualCircle(drawing, x*xScale + zero, y*yScale + zero, 5, normalCol)
	})
	
	// Distance
	val text : VisualText = new VisualText(drawing, 20, 20, "")
	text.setInnerCol(normalCol)
	
	// Frame
	add(drawing)
	pack
	
	override def repaint() {
		
		val xPanel = drawing.getWidth - zero*2
		val yPanel = drawing.getHeight - zero*2
		
		val xScale = xPanel.toDouble / maxX
		val yScale = yPanel.toDouble / maxY
		
		for (i <- Sites) {
			
			val (x, y) = coord(i)
			val (pX, pY) = coord(prev(i))
			
			circles(i).move(x*xScale + zero, y*yScale + zero)
			
			lines(i).setOrig(x*xScale + zero, y*yScale + zero)
			lines(i).setDest(pX*xScale + zero, pY*yScale + zero)
		}
		
		text.setText("Distance : " + dist)
		
		drawing.repaint()
	}

	def update(newPrev : Array[Int], newNext : Array[Int], selected : Array[Boolean]) {
		
		prev = newPrev
		next = newNext
		dist = sumDistances 

		for (i <- Sites) {
			
			if (selected(i))
				circles(i).setInnerCol(selectCol)
			else
				circles(i).setInnerCol(notSelectCol) 
				
			if (selected(i) || selected(prev(i))) 
				lines(i).setOuterCol(selectCol)
			else
				lines(i).setOuterCol(normalCol)
		}
		
		repaint()
	}
	
	def sumDistances = {
		var sum = 0.0
		for (i <- 0 until nSites) sum += distances(i)(prev(i))
		sum
	}
}