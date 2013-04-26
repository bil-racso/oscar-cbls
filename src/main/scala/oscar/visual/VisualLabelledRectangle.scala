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

import java.awt.Color
import java.awt.geom.Line2D
import java.awt.geom.RoundRectangle2D
import java.awt.FontMetrics
import javax.swing.JInternalFrame;
import java.awt.Graphics2D

/**
 * 
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 *
 */
class VisualLabelledRectangle(d: VisualDrawing, shape: RoundRectangle2D.Double, label: String, _marginWidth: Double = 5) extends ColoredShape[RoundRectangle2D.Double](d, shape){
  
  def rect: RoundRectangle2D.Double = shape
  def textDraw = new VisualText(d, (x + marginWidth).toInt, (y + marginWidth + d.getFontMetrics(d.getFont()).getHeight()).toInt, label)
  var marginWidth = _marginWidth
  
  def this(d: VisualDrawing, x: Double, y: Double, label: String, arcw: Double = 7, arch: Double = 7, marginWidth: Double = 5) = {
    this(d,
        new RoundRectangle2D.Double(x,
            y,
            d.getFontMetrics(d.getFont()).stringWidth(label) + marginWidth * 2,
            d.getFontMetrics(d.getFont()).getHeight() + marginWidth * 2,
            arcw,
            arch),
        label,
        marginWidth)
  }
  
  /**
    * X coordinates of bottom left corner
	* @return
	*/
  def x = rect.getX()

  /**
	* Y coordinates of bottom left corner
	* @return
	*/
  def y = rect.getY()
  
  /**
    * X coordinates of bottom left corner
	* @return
	*/
  def xText = (x + marginWidth).toInt

  /**
	* Y coordinates of bottom left corner
	* @return
	*/
  def yText = (y + marginWidth + d.getFontMetrics(d.getFont()).getHeight()).toInt

  /**
	* Move the specified left corner
	* @param x
	*/
  def move(x: Double, y: Double) {
    rect.setRoundRect(x, y, width, height, arcWidth, arcHeight)
    textDraw.move(xText, yText)
	drawing.repaint()
  }
  
  def getWidth(newLabel: String) = {
    d.getFontMetrics(d.getFont()).stringWidth(newLabel) + marginWidth * 2
  }
  
  /**
	* width of the rectangle
	* @return
	*/
  def width = rect.getWidth()
  
  /**
	* height of the rectangle
	* @return
	*/
  def height = rect.getHeight()
	
  /**
	* arc height of the rectangle
	* @return
	*/
  def arcHeight = rect.getArcHeight()
	
  /**
	* width of the rectangle
	* @return
	*/
  def arcWidth = rect.getArcWidth()
  
  /**
	* Set arc width
	* @param arcw
	*/
  def arcWidth_= (arcw: Double) {
	rect.setRoundRect(x, y, width, height, arcw, arcHeight)
	drawing.repaint()
  }
  
  /**
	* Set arc height
	* @param arch
	*/
  def arcHeight_= (arch: Double) {
	rect.setRoundRect(x, y, width, height, arcWidth, arch)
	drawing.repaint()
  }
}

object VisualLabelledRectangle {
  	
  def main(args : Array[String]) {
	val f = new VisualFrame("toto");
	val d = new VisualDrawing(false);
	val inf = f.createFrame("Drawing");
	
	val rect = new VisualLabelledRectangle(d, 50, 50, "Yolo", 10);
	rect.toolTip_$eq("Hello");
	
	inf.add(d);
	f.pack();
			
	Thread.sleep(1000);
	rect.innerCol_$eq(Color.red);
	Thread.sleep(1000);
	rect.move(100, 20);
	for (i <- 0 until 20) {
	  Thread.sleep(50);
	  rect.move(rect.x+5, rect.y);
	}
  }
}