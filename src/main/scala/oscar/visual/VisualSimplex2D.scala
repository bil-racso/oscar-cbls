package oscar.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt
import java.awt.geom.Line2D
import java.awt.Color
import oscar.util.mo.MOOPoint
import org.omg.CORBA.Object
import java.awt.Graphics2D

class VisualSimplex2D[E <% Ordered[E]](d: VisualDrawing, shapes: (Line2D.Double, Line2D.Double, Line2D.Double), labels: (String, String, String)) extends ColoredShape[Line2D.Double](d, shapes._1) {
  
  val textDist = 5
  
  def edges: (Line2D.Double, Line2D.Double, Line2D.Double) = shapes
  
  def this(d: VisualDrawing, simplex: Array[MOOPoint[E]]) {
	this(d, (new Line2D.Double(simplex(0).coordinates(0), simplex(0).coordinates(1), simplex(1).coordinates(0), simplex(1).coordinates(1)),
	    new Line2D.Double(simplex(1).coordinates(0), simplex(1).coordinates(1), simplex(2).coordinates(0), simplex(2).coordinates(1)),
	    new Line2D.Double(simplex(2).coordinates(0), simplex(2).coordinates(1), simplex(0).coordinates(0), simplex(0).coordinates(1))),
	    ("y1", "y2", "y3"))
  }
  
  /**
    * X coordinates of bottom left corner
	* @return
	*/
  def xTexts = ((textDist + edges._1.getX1()).toInt, (textDist + edges._2.getX1()).toInt, (textDist + edges._3.getX1()).toInt)
  
  /**
	* Y coordinates of bottom left corner
	* @return
	*/
  def yTexts = ((textDist + edges._1.getY1()).toInt, (textDist + edges._2.getY1()).toInt, (textDist + edges._3.getY1()).toInt)
  
  override def draw(g: Graphics2D) {
    g.draw(edges._1)
    g.draw(edges._2)
    g.draw(edges._3)
    g.drawString(labels._1, xTexts._1, yTexts._1)
    g.drawString(labels._2, xTexts._2, yTexts._2)
    g.drawString(labels._3, xTexts._3, yTexts._3)
  }
}

object VisualSimplex2D {
  def main(args : Array[String]) {
	val f = new VisualFrame("toto");
	val d = new VisualDrawing(false);
	val inf = f.createFrame("Drawing");
	inf.add(d);
	f.pack();
	
	val simplex = Array(MOOPoint(Array(42.0, 42.0), Array(42.0, 42.0)), MOOPoint(Array(42.0, 0.0), Array(42.0, 42.0)), MOOPoint(Array(0.0, 42.0), Array(42.0, 42.0)))
	val line = new VisualSimplex2D(d, simplex);
	line.toolTip_$eq("Hello");
	
	Thread.sleep(5000);
  }
}
