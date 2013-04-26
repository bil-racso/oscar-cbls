package oscar.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt
import java.awt.geom.Line2D
import java.awt.Color
import oscar.util.mo.MOOPoint

class VisualSimplex2D[E <% Ordered[E]](simplex: Array[MOOPoint[E]], f : (Int) => Int) extends VisualDrawing(false) {
	   												                       	
	private val text : VisualText = new VisualText(this, 50, 50, "")
	text.innerCol = Color.RED
	text.setCentered(true)
	   												               
	private val makespanLine : VisualLine = new VisualLine(this, 0, 0, 0, 0)
	makespanLine.outerCol = Color.RED;
}
