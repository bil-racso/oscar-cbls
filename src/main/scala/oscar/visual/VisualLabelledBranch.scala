/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.visual;

import java.awt.geom.Line2D
import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.Font

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class VisualLabelledBranch(d: VisualDrawing, shapes: (Line2D.Double, Line2D.Double, Line2D.Double), label: String) extends ColoredShape[Line2D.Double](d, shapes._1) {
  
  private val dist = 4
  def branch: (Line2D.Double, Line2D.Double, Line2D.Double) = shapes
  
  def this(d: VisualDrawing, xorig: Double, yorig: Double, xdest: Double, ydest: Double, label: String) {
	this(d, (new Line2D.Double(xorig, yorig, xorig, (ydest + 2 * yorig)/3), new Line2D.Double(xorig, (ydest + 2 * yorig)/3, xdest, (ydest + 2*yorig)/3), new Line2D.Double(xdest, (ydest + 2*yorig)/3, xdest, ydest)), label)
  }
  
  /**
	* Moves to the specified coordinates
	* @param x
	*/
  def move(xorig: Double, yorig: Double, xdest: Double, ydest: Double) {
    branch._1.setLine(xorig, yorig, xorig, (ydest + 2*yorig) / 3)
    branch._2.setLine(xorig, (ydest + 2*yorig) / 3, xdest, (ydest + 2*yorig) / 3)
    branch._3.setLine(xdest, (ydest + 2*yorig) / 3, xdest, ydest)
	drawing.repaint()
  }
  
  /**
    * X coordinates of bottom left corner
	* @return
	*/
  def xText = (dist + branch._3.getX1()).toInt

  /**
	* Y coordinates of bottom left corner
	* @return
	*/
  def yText = ((d.getFontMetrics(d.getFont()).getHeight()) / 2 + (branch._3.getY2() + 2 * branch._3.getY1()) / 3).toInt
  
  override def draw(g: Graphics2D) {
    g.draw(branch._1);
    g.draw(branch._2);
    g.draw(branch._3);
    g.drawString(label, xText, yText)
  }
}

object VisualLabelledBranch {
  	
  def main(args : Array[String]) {
	val f = new VisualFrame("toto");
	val d = new VisualDrawing(false);
	val inf = f.createFrame("Drawing");
	inf.add(d);
	f.pack();
	
	val line = new VisualLabelledBranch(d, 100, 100, 200, 200, "Yolo");
	line.toolTip_$eq("Hello");
	
	Thread.sleep(5000);
  }
}