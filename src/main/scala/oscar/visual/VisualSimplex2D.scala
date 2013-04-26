package oscar.visual

import oscar.visual._
import oscar.cp.scheduling._
import oscar.cp.core.CPVarInt
import java.awt.geom.Line2D
import java.awt.Color
import oscar.util.mo.MOOPoint
import org.omg.CORBA.Object
import java.awt.Graphics2D

class VisualSimplex2D[E <% Ordered[E]](d: VisualDrawing, simplex: Array[MOOPoint[E]]) {
  
  val textDist = 5
  val radius = 2.0
  
  var edges = List[VisualLine]()
  var vertices = List[VisualCircle]()
  var labels = List[VisualText]()
  
  setInitialSimplex(simplex)
  
  def setInitialSimplex(simplex: Array[MOOPoint[E]]) = {
    for (i <- 0 until simplex.length) {
      vertices ::= new VisualCircle(d, simplex(i).coordinates(0), simplex(i).coordinates(1), radius)
      edges ::= new VisualLine(d, new Line2D.Double(simplex(i).coordinates(0), simplex(i).coordinates(1), simplex((i + 1) % 3).coordinates(0), simplex((i + 1) % 3).coordinates(1)))
      labels ::= new VisualText(d, (simplex(i).coordinates(0) + textDist).toInt, (simplex(i).coordinates(1) + textDist).toInt, "y" + i)
    }
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
	val visualSimplex = new VisualSimplex2D(d, simplex);
	
	Thread.sleep(5000);
  }
}
