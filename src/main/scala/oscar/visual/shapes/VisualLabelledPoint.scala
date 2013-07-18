/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.visual.shapes
import java.awt.geom.Ellipse2D
import java.awt.Graphics2D
import java.awt.Color
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame


/**
 * 
 * @author Pierre Schaus
 *
 */
class VisualLabelledPoint(d:VisualDrawing, shape:Ellipse2D.Double, label: String, angle: Double) extends VisualShape[Ellipse2D.Double](d, shape){
  val distFromPoint = 5
  
  def point: Ellipse2D.Double = shape
  def radius = point.getHeight()
  
  def this(d:VisualDrawing, xcenter: Double, ycenter: Double, radius: Double, label: String, angle: Double) {
    this(d, new Ellipse2D.Double(xcenter, ycenter, radius, radius), label, angle)	
  }
  
  def x = point.getX()

  def y = point.getY()
  
  def xText: Int = ((point.getX() + radius + distFromPoint) * math.cos(angle)).toInt
  
  def yText: Int = ((point.getY() + radius + distFromPoint) * math.sin(angle)).toInt
  
  def move(x: Double, y: Double) {
    point.setFrame(x, y, radius, radius)
	drawing.repaint()
  }
  
  override def draw(g: Graphics2D) {
	g.draw(point);
	println(xText + " " + yText)
    g.drawString(label, xText, yText);
  }
}

object VisualLabelledPoint {
  	
  def main(args : Array[String]) {
	val f = VisualFrame("toto");
	val d = VisualDrawing(true);
	val inf = f.createFrame("Drawing");
	inf.add(d);
	f.pack();
	
	val point = new VisualLabelledPoint(d, 200, 0, 1, "Yolo", 180);
	point.toolTip = "Yolo";
			
	Thread.sleep(1000);
	point.innerCol_$eq(Color.red);
	Thread.sleep(1000);
	point.move(100, 100);
	for (i <- 0 until 20) {
	  Thread.sleep(50);
	  point.move(point.x+5, point.y);
	}
  }
}
